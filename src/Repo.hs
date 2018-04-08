module Repo where

import System.Environment (lookupEnv)
import System.IO (FilePath)
import System.Directory (getHomeDirectory, doesDirectoryExist, doesFileExist, listDirectory)
import Data.String.Utils (join, split, strip, splitWs)
import Data.List (isPrefixOf, partition)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Control.Monad hiding (join)
import Text.Regex

data Method = Post | Get | Put | Delete deriving (Show, Read, Eq)

data Endpoint = Endpoint Method Path deriving Show

data RepoType = Service | Library deriving (Show, Eq)

type Path = String
type Name = String

data Repo = Repo { repoName :: Name
                 , repoPath :: FilePath
                 , repoType :: RepoType } deriving (Show)

serviceNameCompleter :: String -> IO [String]
serviceNameCompleter start = do
  allNames <- reponames
  allRepos <- mapM repoFromName allNames
  return $ (catMaybes . fmap collect) allRepos
  where
    collect :: Repo -> Maybe String
    collect (Repo name _ Service) | isMatch name = Just name
    collect _ = Nothing
    acronym = fmap head . split "-"
    isMatch :: String -> Bool
    isMatch nameIn = startC `isPrefixOf` nameInC || startC == acronym nameInC
      where
        startC = fmap toLower start
        nameInC = fmap toLower nameIn

reponames :: IO [String]
reponames = workspaceDir >>= listDirectory

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

repoFromName :: String -> IO Repo
repoFromName name = do
  ws <- workspaceDir
  repoFromDir $ ws ++ "/" ++ name

repoFromDir :: FilePath -> IO Repo
repoFromDir dir = Repo (dropPath dir) dir <$> rtype
  where rtype = do
          a <- doesDirectoryExist $ dir ++ "/app"
          return $ if a then Service else Library

endpointsFromFile :: FilePath -> IO [Endpoint]
endpointsFromFile path = fmap parse <$> endpointsFromFiles' [("",path)]
  where parse :: [String] -> Endpoint
        parse ("/":methodStr:ipath:_)  = Endpoint (readMethod methodStr) ipath
        parse (pref:methodStr:ipath:_) = Endpoint (readMethod methodStr) (pref ++ ipath)
        parse other = error $ "unable to parse '" ++ show other ++ "' into endpoint"

wildcardify :: String -> Regex
wildcardify = mkRegex . replace ":[^/]*" "[^/]*" . replace "[*].*" ".*"
  where replace from to x = subRegex (mkRegex from) x to

matchEndpoint :: [Endpoint] -> Method -> String -> Maybe Endpoint
matchEndpoint [] _ _                        = Nothing
matchEndpoint (   Endpoint m _:epx) method uri | m /= method = matchEndpoint epx method uri
matchEndpoint (ep@(Endpoint _ p):epx) method uri = case wildcardify p `matchRegex` uri of
  Just  _ -> Just ep
  Nothing -> matchEndpoint epx method uri

readMethod :: String -> Method
readMethod (f:fs) = read $ f : fmap toLower fs
readMethod other = error $ "unable to parse '" ++ other ++ "' into method"

endpointsFromFiles' :: [(String, FilePath)] -> IO [[String]]
endpointsFromFiles' [] = return []
endpointsFromFiles' ((prefix, path):xs) = do
  (redirect, clear) <- separate <$> endpointsFromFile' prefix path
  extraFiles        <- filterM (doesFileExist . snd) (makepaths path <$> redirect)
  others            <- endpointsFromFiles' (xs ++ extraFiles)
  return $ others ++ clear
  where
    makepaths :: String -> [String] -> (String, FilePath)
    makepaths p (_:_:pref:route:_) = (prefix ++ pref, takePath p ++ "/" ++ fmap toLower route)
    makepaths e _                  = error $ "cannot parse " ++ show e

separate :: [[String]] -> ([[String]], [[String]])
separate = partition predic
  where
    predic (_:"->":_) = True
    predic _          = False

endpointsFromFile' :: String -> FilePath -> IO [[String]]
endpointsFromFile' prefix file = fmap ((prefix:) . splitWs) . filter notComment . fmap strip . lines <$> readFile file
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
