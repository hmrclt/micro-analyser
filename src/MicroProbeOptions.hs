module MicroProbeOptions where 

import Data.Time

data SearchCriteria = SearchCriteria Int

type Service = String
type SessionId = String

data Environment = Dev | QA | Staging | Prod
                 deriving (Show, Read)

data Options = Options Command Environment DateOpts Mode
             deriving Show

data Command = WhatTalksTo Service Bool Bool
             | WhatItTalksTo Service
             | TraceUser SessionId
             deriving Show

data Mode = Org | DebugResponse | DebugRequest deriving (Show,Read)

data DateOpts = On Day
              | Between UTCTime (Maybe UTCTime)
              | Today
              deriving Show
