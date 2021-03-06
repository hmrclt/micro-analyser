module Main where
import Options.Applicative
import Data.Semigroup((<>))
import Kibana
import MicroProbeOptions
import MAOutput
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Repo
import Table (render)
import MAConfigFile
import MANetwork (assertVpn)

commandParser,whatTalksToParser, whatItTalksToParser, traceUserParser :: Parser Command
whatTalksToParser = WhatTalksTo <$> mainP <*> endpointsBreakdown <*> responseCodeBreakdown
  where
    mainP = argument str (metavar "MICRO-SERVICE" <> help "service name in gov.uk URI - e.g. 'gg'" <> completer (mkCompleter serviceNameCompleter))
    endpointsBreakdown = switch (long "endpoints-breakdown" <> help "group results by endpoint")
    responseCodeBreakdown = switch (long "response-code-breakdown" <> help "group results by response-code")
whatItTalksToParser = WhatItTalksTo <$>
  argument str (metavar "MICRO-SERVICE" <> help "service name in gov.uk URI - e.g. 'gg'")
traceUserParser = TraceUser <$>
  argument str (metavar "SESSION-ID" <> help "SESSION-ID of user")

commandParser = hsubparser (
  command "inbound" (info whatTalksToParser (progDesc "find inbound µS traffic")) <>
  command "outbound" (info whatItTalksToParser (progDesc "find outbound µS traffic")) <>
  command "traceUser" (info traceUserParser (progDesc "trace a user by SESSION-ID"))
  )

environmentParser :: Parser Environment
environmentParser = option auto
          ( help "Which environment to query (Prod|QA|Staging|Dev)"
            <> short 'e'
            <> long "environment"
            <> showDefault
            <> value Prod
            <> metavar "ENVIRONMENT" )

modeParser :: Parser Mode
modeParser = option auto (
  help "Response Mode (Org|Csv|DebugResponse)"
    <> short 'm'
    <> long "mode"
    <> showDefault
    <> value Org
    <> metavar "MODE"
  )

timeoutParser :: Parser Int
timeoutParser = option auto (
  help "timeout in seconds before disconnecting from kibana"
    <> long "timeout"
    <> showDefault
    <> value 10
    <> metavar "SECONDS"
  )

optionParser :: Parser Options
optionParser = Options <$>
  commandParser <*>
  environmentParser <*>
  dateOpts <*>
  modeParser <*>
  timeoutParser

dateOpts :: Parser DateOpts
dateOpts = onParser <|> todayParser <|> rangeParser <|> pure Today
  where onParser = On <$> option auto (long "on" <> metavar "DAY")
        todayParser = flag' Today (long "today" <> help "show todays data")
        rangeParser = Between <$> option auto (long "from" <> metavar "DATETIME")
          <*> option auto (long "to" <> metavar "DATETIME")

main :: IO ()
main = mainProg =<< execParser opts
  where opts = info (optionParser <**> helper) (
          fullDesc <> progDesc "Retrieve µS data from kibana "
                   <> header "micro-probe - a µS diagnostic tool")

mainProg :: Options -> IO ()
mainProg opts = do
  config <- readConfig
  probe opts config

probe :: Options -> MAConfigFile -> IO ()
probe (Options cmd _ dateopts DebugRequest _) _ = do
  (from, to) <- dateRange dateopts
  let query = searchQuery' cmd from to
  BL.putStrLn $ encode query

probe (Options cmd env dateopts DebugResponse timeout) config = do
  (from, to) <- dateRange dateopts
  let query = searchQuery' cmd from to
  _          <- assertVpn $ vpnDevice config
  execQuery' (configSecret config) query env timeout >>= BL.putStrLn . encode

probe (Options cmd env dateopts mode timeout) config = do
  (from, to)   <- dateRange dateopts
  let query = searchQuery' cmd from to
  _            <- assertVpn $ vpnDevice config
  jsonResponse <- execQuery' (configSecret config) query env timeout
  table        <- jsonToTable' jsonResponse cmd
  putStrLn $ render mode table
