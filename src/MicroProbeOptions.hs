module MicroProbeOptions where 

import Data.Time

data SearchCriteria = SearchCriteria Int

type Service = String
type SessionId = String
type TimeoutSeconds = Int

data Environment = Dev | QA | Staging | Prod
                 deriving (Show, Read)

data Options = Options Command Environment DateOpts Mode TimeoutSeconds
             deriving Show

data Command = WhatTalksTo Service Bool Bool
             | WhatItTalksTo Service
             | TraceUser SessionId
             deriving Show

data Mode = Org | Csv | DebugResponse | DebugRequest deriving (Show,Read)

data DateOpts = On Day
              | Between UTCTime (Maybe UTCTime)
              | Today
              deriving Show
