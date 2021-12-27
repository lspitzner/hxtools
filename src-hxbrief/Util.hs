{-# OPTIONS_GHC -Wno-unused-imports #-}

module Util where


import qualified System.Console.ANSI           as Ansi
import           System.Exit                    ( ExitCode
                                                  ( ExitFailure
                                                  , ExitSuccess
                                                  )
                                                )



fGrey :: String
fGrey = Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.White]
fWhite :: String
fWhite = Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.White]
fWhiteDis :: String
fWhiteDis = ""
fRedDis :: String
fRedDis = "" -- TODO disabled until the bug is fixed.
 -- setFGColorDull Ansi.Red

fReset :: String
fReset = Ansi.setSGRCode [Ansi.Reset]

setFGColorVivid :: Ansi.Color -> String
setFGColorVivid c =
  Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Vivid c]
setFGColorDull :: Ansi.Color -> String
setFGColorDull c = Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Dull c]

showEC :: ExitCode -> String
showEC = \case
  ExitSuccess   -> setFGColorVivid Ansi.Green ++ "0" ++ fReset
  ExitFailure i -> setFGColorVivid Ansi.Red ++ show i ++ fReset
