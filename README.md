# micro-analyser

CLI tool for extracting reporting info from Kibana

# Usage

Usage: micro-analyser COMMAND [-e|--environment ENVIRONMENT] ([--on DAY] |
                      [--today] | [--from DATETIME] [--to DATETIME])
                      [-m|--mode MODE] [--timeout SECONDS]
  Retrieve µS data from kibana

Available options:
  -e,--environment ENVIRONMENT
                           Which environment to query
                           (Prod|QA|Staging|Dev) (default: Prod)
  --today                  show todays data
  -m,--mode MODE           Response Mode (Org|Csv|DebugResponse) (default: Org)
  --timeout SECONDS        timeout in seconds before disconnecting from
                           kibana (default: 10)
  -h,--help                Show this help text

Available commands:
  inbound                  find inbound µS traffic
  outbound                 find outbound µS traffic
  traceUser                trace a user by SESSION-ID

# inbound

Usage: micro-analyser inbound MICRO-SERVICE [--endpoints-breakdown]
                              [--response-code-breakdown]
  find inbound µS traffic

Available options:
  MICRO-SERVICE            service name in gov.uk URI - e.g. 'gg'
  --endpoints-breakdown    group results by endpoint
  --response-code-breakdown
                           group results by response-code
  -h,--help                Show this help text

# Setup 

In order to use micro-analyser you will need to extract the authorisation token
from your kibana session and place it in a file called ~/.micro-analyser/secret. 
You only need to do this once. 

1. Open up a browser window with the developer tools. Open the networking tab.
2. Go to Kibana (any environment), log in if necessary and search for something
   (anything)
3. Once _msearch appears in the networking tab of the developer tools open it up
   and save the Authorization part of the request header (without the "Basic"
   prefix)
4. Paste this authorisation string into ~/.micro-analyser/secret using your
   favourite editor
   
You are now good to go.

