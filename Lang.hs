module Lang where

import Data.Char


findall :: Eq a => a -> [(a, b)] -> [b]
findall _ []                   = []
findall x ((k, v):xs) | x == k    = v : findall x xs
                      | otherwise = findall x xs


splitBy :: Eq a => a -> [a] -> [[a]] 
splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs
-- magic, idk how it works

mklang :: String -> [(Char, String)]
mklang s = do
    l <- filter (not . isSpace) <$> lines s
    case l of
        n:'-':'>':r -> do
            r' <- splitBy '|' r
            pure (n, r')
        _ -> error "invalid rule"