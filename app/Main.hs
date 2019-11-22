module Main where

import Text.Printf
import Data.Time
import Data.List

main :: IO ()

main = do
  putStrLn "Enter your numbers! company hours price"
  (year, month, day) <- getCurrentTime >>= return . toGregorian . utctDay
  (company : nums) <- getLine >>= return . words
  let period = intercalate " - " [dateFormat year month 1, dateFormat year month $ gregorianMonthLength year month]
  putStrLn 
    $ foldl (\init item -> init ++ item ++ "\n") ""
      . map (\(x, y) -> x ++ ": " ++ y)
      . zip ["Period", "Company", "Hours", "Price", "Sum"] 
    $ (++) [period, company] 
    $ map show 
      . (\arr -> arr ++ [foldr (*) 1 arr])
      . map (\s -> read s :: Integer)
    $ nums
  where 
    dateFormat = printf "%d-%02d-%02d"
