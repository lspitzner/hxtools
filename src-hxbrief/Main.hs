{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  )
where


import           Control.Concurrent             ( threadDelay )
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.MVar
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
import           Lens.Micro                     ( (<&>) )
import           System.Clock                   ( Clock(RealtimeCoarse)
                                                , TimeSpec
                                                , diffTimeSpec
                                                , getTime
                                                , toNanoSecs
                                                )

import           Data.List                      ( isInfixOf
                                                , isPrefixOf
                                                , isSuffixOf
                                                )
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
import           GHC.IO.Encoding                ( setLocaleEncoding
                                                , utf8
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
import           System.Exit                    ( exitWith )
import           System.IO                      ( Handle
                                                , hClose
                                                , hGetLine
                                                )
import qualified System.Process                as P
import           Text.Printf                    ( printf )
import qualified UI.Butcher.Monadic            as B

import           Util


data StreamKind = StdOut | StdErr
  deriving (Eq, Show)

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
  , c_summarize   :: [String]
  , c_outFile     :: Maybe Handle
  , c_errFile     :: Maybe Handle
  , c_sectionChar :: Maybe Char
  , c_termWidth   :: Maybe Int
  }

-- config TODO:
-- - option to tee to file, or file-out + file-err
-- - parse sections?
-- - allow infix summaries?
-- - drop to "less" environment on error (interactive)

data State = State
  { s_config       :: Config
  , s_regions      :: [ConsoleRegion]
  , s_history      :: [(StreamKind, String)]
  , s_lines        :: [(StreamKind, String)]
  , s_countOut     :: Int
  , s_countErr     :: Int
  , s_globalStart  :: TimeSpec
  , s_lastLineTime :: TimeSpec
  , s_summary :: Maybe ((StreamKind, String), Maybe (Int, String, [String]))
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
dispatchPat oldKind i oldPat prefix = do
  let kindStr = case oldKind of
        StdOut -> "stdout"
        StdErr -> "stderr"
  let betterName =
        let a  = unwords prefix
            la = length a
        in  if
              | i == 1 && la < 70
              -> a
              | la > length oldPat && la < 70
              -> a ++ setFGColorVivid Ansi.Yellow ++ " …" ++ fReset
              | otherwise
              -> showPattern oldPat
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

summarizeLines :: (StreamKind, String) -> StateT State IO ()
summarizeLines cur@(kind, line) = do
  s <- get
  let conf = s_config s
  let match = firstJust
        (\pat -> if matchPattern pat line then Just pat else Nothing)
        (c_summarize conf ++ case kind of
          StdOut -> [ "*" | c_keepStdout conf == Conflate ]
          StdErr -> [ "*" | c_keepStderr conf == Conflate ]
        )
  case (s_summary s, match) of
    (Nothing, _) ->
      put s { s_summary = Just (cur, match <&> \pat -> (1, pat, words line)) }
    (Just (oldLine, Nothing), _) -> do
      dispatchLine oldLine
      put s { s_summary = Just (cur, match <&> \pat -> (1, pat, words line)) }
    (Just ((oldKind, _), Just (i, oldPat, oldPrefix)), Nothing) -> do
      dispatchPat oldKind i oldPat oldPrefix
      put s { s_summary = Just (cur, Nothing) }
    (Just ((oldKind, _), Just (i, oldPat, oldPrefix)), Just pat) -> if
      | oldPat == pat && kind == oldKind -> do
        let newPrefix =
              let go [] = []
                  go ((a, b) : rest) | a == b    = a : go rest
                                     | otherwise = []
              in  go $ zip oldPrefix (words line)
        put s { s_summary = Just (cur, Just (i + 1, pat, newPrefix)) }
      | otherwise -> do
        dispatchPat oldKind i oldPat oldPrefix
        put s { s_summary = Just (cur, Just (1, pat, words line)) }


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
      Just ((StdOut, line), Nothing) ->
        (fWhiteDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdOut, line), Just (1, _, _)) ->
        (fWhiteDis ++ "│ " ++ fReset ++ ellipse line) : prettyLines
      Just ((StdOut, _), Just (i, pat, _)) ->
        (  fWhiteDis
          ++ "│ "
          ++ fReset
          ++ showPattern pat
          ++ " ("
          ++ show i
          ++ " lines)"
          )
          : prettyLines
      Just ((StdErr, line), Nothing) ->
        (fRedDis ++ "│ " ++ ellipse line) : prettyLines
      Just ((StdErr, line), Just (1, _, _)) ->
        (fRedDis ++ "│ " ++ ellipse line) : prettyLines
      Just ((StdErr, _), Just (i, pat, _)) ->
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
  summarize       <- B.addFlagStringParams "s" ["summarize"] "STRING" mempty
  section         <- B.addSimpleBoolFlag "" ["section"] mempty
  B.reorderStop
  rest <- B.addParamRestOfInput "COMMAND" mempty <&> \case
    '-' : '-' : ' ' : r -> r
    '-'       : '-' : r -> r
    r                   -> r
  B.addCmdImpl $ if null rest
    then do
      print "help"
    else withConcurrentOutput $ do
      setLocaleEncoding utf8
      termSizeMay <- Ansi.getTerminalSize
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
      (lastLine, ec) <- displayConsoleRegions $ do
        initialState <- do
          startTime <- getTime RealtimeCoarse
          line0     <- openConsoleRegion Linear
          pure State
            { s_config       = Config
              { c_command     = rest
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
              , c_summarize   = summarize
              , c_outFile     = Nothing
              , c_errFile     = Nothing
              , c_sectionChar = if section then Just '#' else Nothing
              , c_termWidth   = termSizeMay <&> snd
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

        let withCreateProcess f =
              P.withCreateProcess
                  ((P.shell rest) { P.std_in  = P.CreatePipe
                                  , P.std_out = P.CreatePipe
                                  , P.std_err = P.CreatePipe
                                  }
                  )
                $ \(Just inp) (Just out) (Just err) hdl -> do
                    f inp out err hdl

        withCreateProcess $ \inp out err hdl -> do
          hClose inp
          let outHandler = forever $ do
                x <- hGetLine out
                modifyMVar_ stateVar (processLine (StdOut, x))
          let errHandler = forever $ do
                x <- hGetLine err
                modifyMVar_ stateVar (processLine (StdErr, x))
          let tickHandler = forever $ do
                threadDelay 333333
                modifyMVar_ stateVar $ execStateT $ updateStateLine False
          A.withAsync outHandler $ \outAsync ->
            A.withAsync errHandler $ \errAsync ->
              A.withAsync tickHandler $ \_tickAsync -> do
                ec <- P.waitForProcess hdl
                _a <- A.waitCatch outAsync
                _b <- A.waitCatch errAsync
                modifyMVar_ stateVar $ execStateT $ do
                  finalLines <- gets s_lines
                  countOut   <- gets s_countOut
                  countErr   <- gets s_countErr
                  if countOut == 0 && countErr == 1
                    then do
                      modify $ \s ->
                        s { s_config = (s_config s) { c_keepStderr = Keep } }
                      reverse finalLines `forM_` dispatchLine
                    else do
                      -- we leave the lines in final state, but process them
                      reverse finalLines `forM_` summarizeLines
                      gets s_summary >>= \case
                        Nothing              -> pure ()
                        Just (line, Nothing) -> dispatchLine line
                        Just ((kind, _), Just (i, pat, prefix)) ->
                          dispatchPat kind i pat prefix
                finalState <- takeMVar stateVar
                line       <- evalStateT (stateLine False False) finalState
                s_regions finalState `forM_` \r -> closeConsoleRegion r
                -- outputConcurrent (show a ++ "\n")
                -- outputConcurrent (show b ++ "\n")
                pure (fGrey ++ line ++ ", ec=" ++ showEC ec ++ "\n", ec)
      flushConcurrentOutput
      outputConcurrent lastLine
      exitWith ec
