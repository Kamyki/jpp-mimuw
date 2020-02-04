import qualified Data.Char as DC
import Prelude

readInts :: String -> [Int]

readInts [] = []
readInts x = map read (filter (all DC.isDigit) (split x ' '))
    where
        split [] delim = [""]
        split (c:cs) delim
            | c == delim = "" : rest
            | otherwise = (c : head rest) : tail rest
            where
                rest = split cs delim

readInts2 :: String -> Either String [Int]

readInts2 x = fmap id p
    where 
        p
            | all (all DC.isDigit) p1 = Right (map read (filter (all  DC.isDigit) p1))              
            | otherwise = Left ("Eroor " ++ head (filter (not . all DC.isDigit) p1))
        p1 = (split x ' ') 
        split [] delim = [""]
        split (c:cs) delim
            | c == delim = "" : rest
            | otherwise = (c : head rest) : tail rest
            where
                rest = split cs delim
    