module Main where
import Options.Applicative
import Data.Semigroup((<>))
import Kibana
import MicroProbeOptions
import MAOutput
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (catMaybes)
import Table (toOrg, render)

commandParser,whatTalksToParser, whatItTalksToParser, traceUserParser :: Parser Command
whatTalksToParser = WhatTalksTo <$> mainP <*> endpointsBreakdown <*> responseCodeBreakdown
  where
    mainP = argument str (metavar "MICRO-SERVICE" <> help "service name in gov.uk URI - e.g. 'gg'")
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
main = probe =<< execParser opts
  where opts = info (optionParser <**> helper) (
          fullDesc <> progDesc "Retrieve µS data from kibana "
                   <> header "micro-probe - a µS diagnostic tool")

probe :: Options -> IO ()
probe (Options cmd _ dateopts DebugRequest _) = do
  (from, to) <- dateRange dateopts
  let query = searchQuery' cmd from to
  BL.putStrLn $ encode query

probe (Options cmd env dateopts mode timeout) = do
  (from, to) <- dateRange dateopts
  let query = searchQuery' cmd from to 
  jsonResponse <- execQuery' query env timeout
  putStrLn $ render mode $ jsonToTable jsonResponse headers 
  where
    headers = case cmd of
      (WhatTalksTo _ a b) -> catMaybes [Just "Caller"
                                       , if a then Just "Endpoint" else Nothing
                                       , if b then Just "Response" else Nothing
                                       ]
      _ -> []
