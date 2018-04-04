module Repo where

import System.Environment (lookupEnv)
import System.IO (FilePath)
import System.Directory (getHomeDirectory, doesDirectoryExist)
import Data.String.Utils (join, split, strip, splitWs)
import Data.List (isPrefixOf)
-- import Data.Char (toLower)

data Method = Post | Get | Put | Delete deriving (Show, Read)

data Endpoint = Endpoint Method Path deriving Show

data RepoType = Service [Endpoint] | Library deriving Show

type Path = String
type Name = String

data Repo = Repo Name FilePath RepoType deriving (Show)

workspaceDir :: IO FilePath
workspaceDir = do
  ws <- lookupEnv "WORKSPACE"
  case ws of
    Just a -> return a
    Nothing -> (++"/hmrc") <$> getHomeDirectory 

dropPath :: FilePath -> FilePath
dropPath path = reverse $ inner path []
  where
    inner [] acc = acc
    inner ('/':xs) _ = inner xs []
    inner (x:xs) acc = inner xs (x:acc)

takePath :: FilePath -> FilePath
takePath = join "/" . init . split "/"

repoFromDir :: FilePath -> IO Repo
repoFromDir dir = Repo (dropPath dir) dir <$> rtype
  where rtype = do
          a <- doesDirectoryExist $ dir ++ "/app"
          return $ if a then Service [] else Library

endpointsFromFiles :: [(String, FilePath)] -> IO [[String]]
endpointsFromFiles [] = return []
endpointsFromFiles ((prefix, path):xs) = do
  (clear, redirect) <- separate <$> endpointsFromFile prefix path
  others <- endpointsFromFiles xs
  return $ others ++ clear
  where
    separate :: [[String]] -> ([[String]], [[String]])
    separate = undefined

endpointsFromFile :: String -> FilePath -> IO [[String]]
endpointsFromFile prefix file = fmap ((prefix:) . splitWs) . filter notComment . fmap strip . lines <$> readFile file
  where
    notComment :: String -> Bool
    notComment l = l /= "" && not ("#" `isPrefixOf` l)

-- endpointsFromFile :: FilePath -> IO [[String]]
-- endpointsFromFile file = toEndpoint <$> fmap (splitWs) . filter notComment . fmap strip . lines <$> readFile file
--   where
--     notComment :: String -> Bool
--     notComment l = l /= "" && not ("#" `isPrefixOf` l)
--     toEndpoint :: [String] -> IO [[String]]
--     toEndpoint ("->":_:c:_) = endpointsFromFile $ takePath file ++ "/" ++ (fmap toLower c)
--     toEndpoint (m:p:_) = return [(read m):[p]]
--     toEndpoint _ = return []

endpointsFromProjectDir :: FilePath -> IO [Endpoint]
endpointsFromProjectDir = undefined
