module MyPrelude where


import Data.List



listIndices :: [a] -> [Int]
listIndices = findIndices (const True)
