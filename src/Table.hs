module Table ( Table
             , table
             , header
             , headerK
             , colK
             , row
             , col
             , rowM
             , colM
             , cell
             , cellM
             , headMap
             , headers
             , body
             , toCsv
             , toOrg
             , mapCol
             , mapColK             
             )  where 

import Data.Vector hiding ((++), mapM_)
import Data.Maybe (fromJust)
import Data.Functor()
import Data.List (intercalate)

data Table h a = Table { headers :: Vector h
                       , body    :: Vector (Vector a)
                       } deriving (Show,Read)

table :: [h] -> [[a]] -> Table h a
table h b = Table (fromList h) (fromList (fromList <$> b))

header :: Table h a -> Int -> h
header tab i = headers tab ! i

headerK :: Eq h => Table h a -> h -> Int
headerK tab i = fromJust $ elemIndex i (headers tab)

colK :: Eq h => Table h a -> h -> [a]
colK tab key = col tab (headerK tab key)

row, col :: Table h a -> Int -> [a]
rowM, colM :: Table h a -> Int -> Maybe [a]
cell :: Table h a -> Int -> Int -> a
cellM :: Table h a -> Int -> Int -> Maybe a

row tab i = toList $ body tab ! i
rowM tab i = toList <$> body tab !? i
col tab i = toList $ fmap (! i) body tab
colM tab i = Prelude.sequence $ toList $ fmap (!? i) (body tab)

cell tab r c = body tab ! r ! c
cellM tab r c = body tab !? r >>= (!? c)

headMap :: (h -> b) -> Table h a -> Table b a
headMap f (Table h b) = Table (fmap f h) b

mapCol :: (a -> a) -> Int -> Table h a -> Table h a
mapCol f hk (Table h b) = Table h newB
  where newB = fmap updateLine b
        updateLine l = fromList $ updateC f <$> (toList . indexed) l
        updateC :: (a -> a) -> (Int, a) -> a
        updateC f2 (i,v) | i == hk = f2 v
        updateC _ (_,v)           = v        

mapColK :: Eq h => (a -> a) -> h -> Table h a -> Table h a
mapColK f k t = mapCol f (headerK t k) t

instance Functor (Table h) where  
    fmap f (Table h b) = Table h (fmap (fmap f) b)

toCsv :: (Show h, Show a) => Table h a -> String
toCsv (Table h b) = intercalate "\n" allLines
  where line cells = intercalate "," $ (fmap show . toList) cells
        allLines = line h : (toList . fmap line) b

toOrg :: (Show h, Show a) => Table h a -> String
toOrg (Table h b) = intercalate "\n" allLines
  where line cells = "| " ++ intercalate " | " ((fmap show . toList) cells) ++ " |"
        allLines = line h : "|-" : (toList . fmap line) b
