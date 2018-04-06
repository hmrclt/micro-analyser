module Table ( Table
             , table
             , header
             , headerK
             , colK
             , row
             , col
             , rowM
             , colM
             , colsV
             , cols
             , width
             , height
             , foldlRows
             , foldlCols
             , foldrRows
             , foldrCols
             , cell
             , cellM
             , headMap
             , headers
             , rowsV
             , toCsv
             , toOrg
             , mapCol
             , mapColK
             , render
             ) where

import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import Data.Maybe (fromJust)
import Data.Functor()
import Data.List (intercalate, transpose)
import MicroProbeOptions (Mode(Org, Csv))

data Table h a = Table { headersV :: V.Vector h
                       , rowsV   :: V.Vector (V.Vector a)
                       } deriving (Show,Read)

instance Functor (Table h) where
    fmap f (Table h b) = Table h (fmap (fmap f) b)

colsV :: Table h a -> V.Vector (V.Vector a)
colsV = V.fromList . fmap V.fromList . transpose . V.toList . fmap V.toList . rowsV

headers :: Table h a -> [h]
headers = V.toList . headersV

rows, cols :: Table h a -> [[a]]
rows = V.toList . fmap V.toList . rowsV
cols = transpose . rows

width, height :: Table h a -> Int
width  = V.foldl max 0 . fmap V.length . rowsV
height = Prelude.foldl max 0 . fmap Prelude.length . cols

table :: [h] -> [[a]] -> Table h a
table h b = Table (V.fromList h) (V.fromList (V.fromList <$> b))

header :: Table h a -> Int -> h
header tab i = headersV tab ! i

headerK :: Eq h => Table h a -> h -> Int
headerK tab i = fromJust $ V.elemIndex i (headersV tab)

colK :: Eq h => Table h a -> h -> [a]
colK tab key = col tab (headerK tab key)

row, col :: Table h a -> Int -> [a]
rowM, colM :: Table h a -> Int -> Maybe [a]
cell :: Table h a -> Int -> Int -> a
cellM :: Table h a -> Int -> Int -> Maybe a

row tab i = V.toList $ rowsV tab ! i
rowM tab i = V.toList <$> rowsV tab !? i
col tab i = V.toList $ fmap (! i) rowsV tab
colM tab i = Prelude.sequence $ V.toList $ fmap (!? i) (rowsV tab)

cell tab r c = rowsV tab ! r ! c
cellM tab r c = rowsV tab !? r >>= (!? c)

headMap :: (h -> b) -> Table h a -> Table b a
headMap f (Table h b) = Table (fmap f h) b

mapCol :: (a -> a) -> Int -> Table h a -> Table h a
mapCol f hk (Table h b) = Table h newB
  where newB = fmap updateLine b
        updateLine l = V.fromList $ updateC f <$> (V.toList . V.indexed) l
        updateC :: (a -> a) -> (Int, a) -> a
        updateC f2 (i,v) | i == hk = f2 v
        updateC _ (_,v)           = v

mapColK :: Eq h => (a -> a) -> h -> Table h a -> Table h a
mapColK f k t = mapCol f (headerK t k) t

foldlRows, foldlCols :: (b -> a -> b) -> b -> Table h a -> [b]
foldlRows f i tab = V.toList $ fmap (V.foldl f i) (rowsV tab)
foldlCols f i tab = fmap (Prelude.foldl f i) (cols tab)

foldrRows, foldrCols :: (a -> b -> b) -> b -> Table h a -> [b]
foldrRows f i tab = V.toList $ fmap (V.foldr f i) (rowsV tab)
foldrCols f i tab = fmap (Prelude.foldr f i) (cols tab)

toCsv :: (Show h, Show a) => Table h a -> String
toCsv (Table h b) = intercalate "\n" allLines
  where line cells = intercalate "," $ (fmap show . V.toList) cells
        allLines = line h : (V.toList . fmap line) b

presentationTable' :: Table String String -> [[String]]
presentationTable' tab = fmap padRow smooshit
  where
    padRow = Prelude.zipWith padL colWidths
    smooshit :: [[String]]
    smooshit = headers tab : rows tab
    colWidths = (fmap (Prelude.foldl (\a b -> max a (Prelude.length b)) 0) . transpose) smooshit
    padL :: Int -> String -> String
    padL n s
      | Prelude.length s < n  = s ++ Prelude.replicate (n - Prelude.length s) ' '
      | otherwise = s


presentationTable :: (Show h, Show a) => Table h a -> [[String]]
presentationTable tab = fmap padRow smooshit
  where
    padRow :: [String] -> [String]
    padRow = Prelude.zipWith padL colWidths
    smooshit :: [[String]]
    smooshit = (fmap show . headers ) tab : (rows . fmap show) tab
    colWidths :: [Int]
    colWidths = (fmap (Prelude.foldl (\a b -> max a (Prelude.length b)) 0) . transpose) smooshit
    padL :: Int -> String -> String
    padL n s
      | Prelude.length s < n  = s ++ Prelude.replicate (n - Prelude.length s) ' '
      | otherwise = s

toOrg :: (Show h, Show a) => Table h a -> String
toOrg tab = intercalate "\n" allLines
  where allLines = (toLine . Prelude.head) pres : separator : (fmap toLine .Prelude.tail) pres
        pres = presentationTable tab
        toLine = toLine' "| " " | " " |"
        toLine' pref inf suf x = pref ++ intercalate inf x ++ suf
        separator = toLine' "|-" "-+-" "-|" $ fmap (const '-') <$> Prelude.head pres

render :: Mode -> Table String String -> String
render Org tab = intercalate "\n" allLines
  where allLines = toLine presH : separator : fmap toLine presT
        (presH:presT) = presentationTable' tab
        toLine = toLine' "| " " | " " |"
        toLine' pref inf suf x = pref ++ intercalate inf x ++ suf
        separator = toLine' "|-" "-+-" "-|" $ fmap (const '-') <$> presH
render Csv tab = intercalate "\n" allLines
  where allLines = intercalate "," <$> headers tab : rows tab
render r _ = error "Bad renderer: " ++ show r
