{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  )
where


import           Control.Concurrent             ( threadDelay )
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.MVar
import           Control.Exception              ( AsyncException(UserInterrupt)
                                                , IOException
                                                , bracket
                                                , catch
                                                , mask
                                                , throwIO
                                                , try
                                                )
import           Control.Monad                  ( forM_
                                                , forever
                                                , replicateM
                                                , when
                                                , zipWithM_
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.State.Strict
                                                ( StateT(StateT)
                                                , evalStateT
                                                , execStateT
                                                , get
                                                , gets
                                                , modify
                                                , put
                                                )
import qualified Data.Char                     as Char
import           Data.List                      ( isInfixOf
                                                , isPrefixOf
                                                , isSuffixOf
                                                )
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
import qualified GHC.IO.Encoding
import           Lens.Micro                     ( (<&>) )
import           System.Clock                   ( Clock(RealtimeCoarse)
                                                , TimeSpec
                                                , diffTimeSpec
                                                , getTime
                                                , toNanoSecs
                                                )
import qualified System.Console.ANSI           as Ansi
import           System.Console.Concurrent      ( errorConcurrent
                                                , flushConcurrentOutput
                                                , outputConcurrent
                                                , withConcurrentOutput
                                                )
import           System.Console.Regions         ( ConsoleRegion
                                                , RegionLayout(Linear)
                                                , closeConsoleRegion
                                                , displayConsoleRegions
                                                , openConsoleRegion
                                                , setConsoleRegion
                                                )
import qualified System.Environment
import           System.Exit                    ( exitSuccess
                                                , exitWith
                                                )
import           System.IO                      ( Handle )
import qualified System.IO
import qualified System.Process                as P
import           Text.Printf                    ( printf )
import qualified UI.Butcher.Monadic            as B

import           Util


data StreamKind = StdOut | StdErr
  deriving (Eq, Show)

data JoinMode
  = JoinAll
  | JoinSpecific

data JoinedInfo
  = JoinedNot
  | JoinedAll Int
  | Joined Int String [String] -- pattern, prefix

data KeepMode
  = Drop -- dont forward
  | Keep -- forward each line, apart from summaries
  | Conflate -- summarize non-summarized lines as "*"
  deriving (Eq, Show)

data Config = Config
  { c_command     :: String
  , c_lines       :: Int
  , c_keepStdout  :: KeepMode
  , c_keepStderr  :: KeepMode
  , c_summarize   :: [(JoinMode, String)]
  , c_outFile     :: Maybe Handle
  , c_errFile     :: Maybe Handle
  , c_sectionChar :: Maybe Char
  , c_termWidth   :: Maybe Int
  }

data State = State
  { s_config       :: Config
  , s_regions      :: [ConsoleRegion]
  , s_history      :: [(StreamKind, String)]
  , s_lines        :: [(StreamKind, String)]
  , s_countOut     :: Int
  , s_countErr     :: Int
  , s_globalStart  :: TimeSpec
  , s_lastLineTime :: TimeSpec
  , s_summary      :: Maybe ((StreamKind, String), JoinedInfo)
  }


getTimeDiff :: Bool -> StateT State IO (Float, Float)
getTimeDiff updateCur = do
  now <- liftIO $ getTime RealtimeCoarse
  when updateCur $ modify $ \s -> s { s_lastLineTime = now }
  s <- get
  let diffNanos1 = toNanoSecs $ diffTimeSpec (s_globalStart s) now
  let diffNanos2 = toNanoSecs $ diffTimeSpec (s_lastLineTime s) now
  pure
    ( fromIntegral (diffNanos1 `div` 1000000) / (1000 :: Float)
    , fromIntegral (diffNanos2 `div` 1000000) / (1000 :: Float)
    )


stateLine :: Bool -> Bool -> StateT State IO String
stateLine updateCur showCur = do
  (diffFloat1, diffFloat2) <- getTimeDiff updateCur
  s                        <- get
  let outStr = if showCur && diffFloat2 > 1.0
        then printf
          "waiting since %0.0fs … %0.1fs total, %i/%i lines stdout/stderr"
          diffFloat2
          diffFloat1
          (s_countOut s)
          (s_countErr s)
        else printf "%0.3fs total, %i lines stdout %i lines stderr"
                    diffFloat1
                    (s_countOut s)
                    (s_countErr s)

  pure $ fGrey ++ outStr

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

matchPattern :: String -> String -> Bool
matchPattern pat s = case break (== '*') pat of
  ("", "*"       ) -> True
  ("", '*' : rest) -> case break (== '*') rest of
    (start, "*") -> start `isInfixOf` s
    (_    , "" ) -> rest `isSuffixOf` s
    _            -> error $ "too many globs in pattern " ++ pat ++ "!"
  (start, '*' : rest) -> if any (== '*') rest
    then error "only one glob supported in patterns!"
    else start `isPrefixOf` s && rest `isSuffixOf` s
  ("", "") -> error "empty pattern"
  _        -> undefined

dispatchLine :: (StreamKind, String) -> StateT State IO ()
dispatchLine line@(kind, str) = do
  conf <- gets s_config
  liftIO $ case kind of
    StdOut -> when (c_keepStdout conf /= Drop)
      $ outputConcurrent (fReset ++ str ++ "\n")
    StdErr ->
      when (c_keepStderr conf /= Drop) $ errorConcurrent (fReset ++ str ++ "\n")
  modify $ \s -> s { s_history = line : s_history s }

showPattern :: String -> String
showPattern p = p >>= \case
  '*' -> setFGColorVivid Ansi.Yellow ++ "…" ++ fReset
  x   -> [x]

dispatchPat :: StreamKind -> Int -> String -> [String] -> StateT State IO ()
dispatchPat oldKind i pat prefix = do
  let kindStr = case oldKind of
        StdOut -> "stdout"
        StdErr -> "stderr"
  let betterName =
        let a  = unwords prefix
            la = length a
        in  if
              | i == 1 && la < 70
              -> a
              | la > length pat && la < 70
              -> a ++ setFGColorVivid Ansi.Yellow ++ " …" ++ fReset
              | otherwise
              -> showPattern pat
  let prettyPat =
        fGrey
          ++ "("
          ++ show i
          ++ " lines "
          ++ kindStr
          ++ ")"
          ++ fReset
          ++ " "
          ++ betterName
          ++ "\n"
  conf <- gets s_config
  liftIO $ case oldKind of
    StdOut -> when (c_keepStdout conf /= Drop) $ outputConcurrent prettyPat
    StdErr -> when (c_keepStderr conf /= Drop) $ errorConcurrent prettyPat

dispatchSkipped :: StreamKind -> Int -> StateT State IO ()
dispatchSkipped oldKind i = do
  let kindStr = case oldKind of
        StdOut -> "stdout"
        StdErr -> "stderr"
  let prettyPat =
        fGrey
          ++ "("
          ++ show i
          ++ " lines "
          ++ kindStr
          ++ ")"
          ++ fReset
          ++ " …skipped…\n"
  conf <- gets s_config
  liftIO $ case oldKind of
    StdOut -> when (c_keepStdout conf /= Drop) $ outputConcurrent prettyPat
    StdErr -> when (c_keepStderr conf /= Drop) $ errorConcurrent prettyPat


summarizeLines :: (StreamKind, String) -> StateT State IO ()
summarizeLines cur@(kind, line) = do
  s <- get
  let conf = s_config s
  let match = firstJust
        (\joiner@(_, pat) ->
          if matchPattern pat line then Just joiner else Nothing
        )
        (c_summarize conf ++ case kind of
          StdOut -> [ (JoinAll, "*") | c_keepStdout conf == Conflate ]
          StdErr -> [ (JoinAll, "*") | c_keepStderr conf == Conflate ]
        )
  case (s_summary s, match) of
    (Nothing, _) -> put s
      { s_summary = Just
                      ( cur
                      , case match of
                        Nothing                  -> JoinedNot
                        Just (JoinAll     , _  ) -> JoinedAll 1
                        Just (JoinSpecific, pat) -> Joined 1 pat (words line)
                      )
      }
    (Just (oldLine, JoinedNot), _) -> do
      dispatchLine oldLine
      put s
        { s_summary = Just
                        ( cur
                        , case match of
                          Nothing                  -> JoinedNot
                          Just (JoinAll     , _  ) -> JoinedAll 1
                          Just (JoinSpecific, pat) -> Joined 1 pat (words line)
                        )
        }
    (Just ((oldKind, _), JoinedAll i), Nothing) -> do
      dispatchSkipped oldKind i
      put s { s_summary = Just (cur, JoinedNot) }
    (Just ((oldKind, _), Joined i oldPat oldPrefix), Nothing) -> do
      dispatchPat oldKind i oldPat oldPrefix
      put s { s_summary = Just (cur, JoinedNot) }
    (Just ((oldKind, _), JoinedAll i), Just joiner) -> case joiner of
      (JoinAll, _)
        | kind == oldKind -> do
          put s { s_summary = Just (cur, JoinedAll (i + 1)) }
        | otherwise -> do
          dispatchSkipped oldKind i
          put s { s_summary = Just (cur, JoinedAll 1) }
      (JoinSpecific, pat) -> do
        dispatchSkipped oldKind i
        put s { s_summary = Just (cur, Joined 1 pat (words line)) }
    (Just ((oldKind, _), Joined i oldPat oldPrefix), Just joiner) ->
      case joiner of
        (JoinSpecific, pat) | oldPat == pat && kind == oldKind -> do
          let newPrefix =
                let go [] = []
                    go ((a, b) : rest) | a == b    = a : go rest
                                       | otherwise = []
                in  go $ zip oldPrefix (words line)
          put s { s_summary = Just (cur, Joined (i + 1) pat newPrefix) }
        _ -> do
          dispatchPat oldKind i oldPat oldPrefix
          put s
            { s_summary = Just
                            ( cur
                            , case joiner of
                              (JoinAll     , _  ) -> JoinedAll 1
                              (JoinSpecific, pat) -> Joined 1 pat (words line)
                            )
            }


processLine :: (StreamKind, String) -> State -> IO State
processLine newPair@(kind, _) = execStateT $ do
  conf <- gets s_config
  modify $ \s -> s { s_lines = newPair : s_lines s }
  do
    s0 <- get
    let (keep, over) = splitAt (c_lines conf - 1) (s_lines s0)
    put s0 { s_lines = keep }
    over `forM_` summarizeLines
  case kind of
    StdOut -> modify $ \s -> s { s_countOut = s_countOut s + 1 }
    StdErr -> modify $ \s -> s { s_countErr = s_countErr s + 1 }
  curLines               <- gets s_lines
  prettyLinesWithSummary <- do
    let ellipse =
          let go _ ""       = ""
              go 0 _        = "…"
              go n (x : xs) = x : go (n - 1) xs
          in  case c_termWidth conf of
                Nothing -> id
                Just w  -> go (w - 3)
    let prettyLines = reverse $ take (c_lines conf) curLines <&> \case
          (StdOut, line) -> fWhiteDis ++ "│ " ++ fReset ++ ellipse line
          (StdErr, line) -> fRedDis ++ "│ " ++ fReset ++ ellipse line
    summary <- gets s_summary
    pure $ case summary of
      Nothing -> prettyLines
      Just ((StdOut, line), JoinedNot) ->
        (fWhiteDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdOut, line), JoinedAll 1) ->
        (fWhiteDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdOut, _line), JoinedAll i) ->
        (fWhiteDis ++ "│ " ++ fReset ++ "…skipped… (" ++ show i ++ " lines)")
          : prettyLines
      Just ((StdOut, line), Joined 1 _ _) ->
        (fWhiteDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdOut, _), Joined i pat _) ->
        (  fWhiteDis
          ++ "│ "
          ++ fReset
          ++ showPattern pat
          ++ " ("
          ++ show i
          ++ " lines)"
          )
          : prettyLines
      Just ((StdErr, line), JoinedNot) ->
        (fRedDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdErr, line), JoinedAll 1) ->
        (fRedDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdErr, _line), JoinedAll i) ->
        (fRedDis ++ "│ " ++ fReset ++ "…skipped… (" ++ show i ++ " lines)")
          : prettyLines
      Just ((StdErr, line), Joined 1 _ _) ->
        (fRedDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdErr, _), Joined i pat _) ->
        (  fRedDis
          ++ "│ "
          ++ fReset
          ++ showPattern pat
          ++ " ("
          ++ show i
          ++ " lines)"
          )
          : prettyLines
  let showCount = min (c_lines conf) (length prettyLinesWithSummary)
  do -- make sure we have enough regions allocated
    let need = showCount + 1
    got <- gets (length . s_regions)
    when (need > got) $ do
      new <- liftIO $ replicateM (need - got) (openConsoleRegion Linear)
      modify $ \s -> s { s_regions = s_regions s ++ new }
  do
    regions <- gets s_regions
    liftIO $ zipWithM_
      (\x region -> do
        setConsoleRegion region x
      )
      prettyLinesWithSummary
      (take showCount regions)
  updateStateLine True

updateStateLine :: Bool -> StateT State IO ()
updateStateLine updateCur = do
  line <- stateLine updateCur True
  s    <- get
  liftIO $ setConsoleRegion
    (last $ s_regions s)
    (  fGrey
    ++ "╰─ … "
    ++ line
    ++ " "
    ++ setFGColorVivid Ansi.Blue
    ++ (c_command $ s_config s)
    ++ fReset
    )

quoteIfSpaces :: String -> String
quoteIfSpaces s = if any Char.isSpace s then "\"" ++ s ++ "\"" else s

main :: IO ()
main = B.mainFromCmdParser $ do
  B.reorderStart
  numLines :: Int <- B.addFlagReadParam "n" ["lines"] "LINES" (B.flagDefault 5)
  keepStdout      <- B.addSimpleBoolFlag "" ["keep-out"] mempty
  keepStderr      <- B.addSimpleBoolFlag "" ["keep-err"] mempty
  keepBoth        <- B.addSimpleBoolFlag "" ["keep"] mempty
  dropStdout      <- B.addSimpleBoolFlag "" ["drop-out"] mempty
  dropStderr      <- B.addSimpleBoolFlag "" ["drop-err"] mempty
  dropBoth        <- B.addSimpleBoolFlag "" ["drop"] mempty
  conflateStdout  <- B.addSimpleBoolFlag "" ["conflate-out"] mempty
  conflateStderr  <- B.addSimpleBoolFlag "" ["conflate-err"] mempty
  conflateBoth    <- B.addSimpleBoolFlag "" ["conflate"] mempty
  summarize       <- B.addFlagStringParams "s" ["summarize"] "PATTERN" mempty
  skip            <- B.addFlagStringParams "x" ["skip"] "PATTERN" mempty
  -- section         <- B.addSimpleBoolFlag "" ["section"] mempty
  B.reorderStop
  rest <- B.addParamRestOfInputRaw "COMMAND" mempty <&> \case
    B.InputString ('-' : '-' : ' ' : r) -> words r
    B.InputString ('-'       : '-' : r) -> words r
    B.InputString (r                  ) -> words r
    B.InputArgs   ("--" : r           ) -> r
    B.InputArgs   r                     -> r
  helpDesc <- B.peekCmdDesc
  B.addCmdImpl $ do
    when (null rest) $ do
      print $ B.ppHelpShallow helpDesc
      exitSuccess
    let (restPath : restArgs) = rest
    recursiveMay <- System.Environment.lookupEnv "IN_HXBRIEF"
    case recursiveMay of
      Just _ -> do
        -- TODO: Arguably, we should do _something_ here, e.g. summarizing
        --       and filtering etc.
        P.callProcess restPath restArgs
        exitSuccess
      Nothing -> pure ()
    let mainBracket = bracket
          (do
            System.IO.hSetEcho System.IO.stdin False
            System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
            GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
            pure ()
          )
          (\() -> do
            System.IO.hSetEcho System.IO.stdin True
          )
    withConcurrentOutput $ mainBracket $ \() -> mask $ \restore -> do
      -- restore $ GHC.IO.Encoding.setFileSystemEncoding GHC.IO.Encoding.utf8
      -- restore $ System.IO.hSetEncoding System.IO.stdout GHC.IO.Encoding.utf8
      -- restore $ System.IO.hSetEncoding System.IO.stderr GHC.IO.Encoding.utf8
      termWidthMay <- restore $ do
        support <- Ansi.hSupportsANSI System.IO.stdin
        if support then fmap snd <$> Ansi.getTerminalSize else pure Nothing
      let stdoutCheckCount =
            length
              $  [ () | keepStdout || keepBoth ]
              ++ [ () | conflateStdout || conflateBoth ]
              ++ [ () | dropStdout || dropBoth ]
      let stderrCheckCount =
            length
              $  [ () | keepStderr || keepBoth ]
              ++ [ () | conflateStderr || conflateBoth ]
              ++ [ () | dropStderr || dropBoth ]
      (lastLine, ecMay) <- displayConsoleRegions $ do
        initialState <- do
          startTime <- getTime RealtimeCoarse
          line0     <- openConsoleRegion Linear
          pure State
            { s_config       = Config
              { c_command     = unwords $ map quoteIfSpaces rest
              , c_lines       = numLines
              , c_keepStdout  = if
                                  | stdoutCheckCount > 1 -> error
                                    "too many keep/drop/conflate for stdout!"
                                  | keepStdout || keepBoth -> Keep
                                  | conflateStdout || conflateBoth -> Conflate
                                  | dropStdout || dropBoth -> Drop
                                  | otherwise -> if null summarize
                                    then Drop
                                    else Conflate
              , c_keepStderr  = if
                                  | stderrCheckCount > 1 -> error
                                    "too many keep/drop/conflate for stderr!"
                                  | keepStderr || keepBoth -> Keep
                                  | conflateStderr || conflateBoth -> Conflate
                                  | dropStderr || dropBoth -> Drop
                                  | otherwise -> Keep
              , c_summarize   = (summarize <&> \x -> (JoinSpecific, x))
                                  ++ (skip <&> \x -> (JoinAll, x))
              , c_outFile     = Nothing
              , c_errFile     = Nothing
              , c_sectionChar = Nothing -- if section then Just '#' else Nothing
              , c_termWidth   = termWidthMay
              }
            , s_regions      = [line0]
            , s_history      = []
            , s_lines        = []
            , s_countOut     = 0
            , s_countErr     = 0
            , s_globalStart  = startTime
            , s_lastLineTime = startTime
            , s_summary      = Nothing
            }
        stateVar :: MVar State <- newMVar initialState

        let inHandler inp =
              let go = do
                    x <- try getLine
                    case x of
                      Left  (_ :: IOException) -> System.IO.hClose inp
                      Right line               -> do
                        System.IO.hPutStrLn inp line
                        System.IO.hFlush inp
                        go
              in  go
        let outHandler out = forever $ do
              x <- System.IO.hGetLine out
              modifyMVar_ stateVar (processLine (StdOut, x))
        let errHandler err = forever $ do
              x <- System.IO.hGetLine err
              modifyMVar_ stateVar (processLine (StdErr, x))
        let tickHandler = forever $ do
              threadDelay 333333
              modifyMVar_ stateVar $ execStateT $ updateStateLine False
        innerEnv <- do
          env <- System.Environment.getEnvironment
          pure (env ++ [("IN_HXBRIEF", "1")])

        let mainBlock =
              P.withCreateProcess
                  ((P.proc restPath restArgs) { P.std_in  = P.CreatePipe
                                              , P.std_out = P.CreatePipe
                                              , P.std_err = P.CreatePipe
                                              , P.env     = Just innerEnv
                                              }
                  )
                $ \(Just inp) (Just out) (Just err) hdl -> do
                    A.withAsync (inHandler inp) $ \inAsync ->
                      A.withAsync (outHandler out) $ \outAsync ->
                        A.withAsync (errHandler err) $ \errAsync ->
                          A.withAsync tickHandler $ \_tickAsync -> do
                            ec <- P.waitForProcess hdl
                            A.cancel inAsync
                            _a <- A.waitCatch outAsync
                            _b <- A.waitCatch errAsync
                            pure (Just ec)
        ecMay <- restore mainBlock `catch` (\UserInterrupt -> pure Nothing)
        modifyMVar_ stateVar $ execStateT $ do
          finalLines <- gets s_lines
          countOut   <- gets s_countOut
          countErr   <- gets s_countErr
          if countOut == 0 && countErr == 1
            then do
              modify
                $ \s -> s { s_config = (s_config s) { c_keepStderr = Keep } }
              reverse finalLines `forM_` dispatchLine
            else do
              -- we leave the lines in final state, but process them
              reverse finalLines `forM_` summarizeLines
              gets s_summary >>= \case
                Nothing                       -> pure ()
                Just (line     , JoinedNot  ) -> dispatchLine line
                Just ((kind, _), JoinedAll i) -> dispatchSkipped kind i
                Just ((kind, _), Joined i pat prefix) ->
                  dispatchPat kind i pat prefix
        finalState <- takeMVar stateVar
        line       <- evalStateT (stateLine False False) finalState
        s_regions finalState `forM_` \r -> closeConsoleRegion r
        let lastLine = case ecMay of
              Nothing -> fGrey ++ line ++ ", UserInterrupt\n" ++ fReset
              Just ec -> fGrey ++ line ++ ", ec=" ++ showEC ec ++ "\n"
        pure (lastLine, ecMay)

      flushConcurrentOutput
      outputConcurrent lastLine
      case ecMay of
        Nothing -> throwIO UserInterrupt -- essentially re-throw
        Just ec -> exitWith ec

