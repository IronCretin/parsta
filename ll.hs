{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.RWS.Lazy
import Data.DList (DList)
import qualified Data.DList as DList
import Data.List
import Data.Char
import System.Environment

import Stack
import Lang


llstep :: RWST [(Char, String)] (DList String) (String, String) [] ()
llstep = do
    (inp, st) <- get
    tell $ DList.singleton $ " (" ++ inp ++ ", " ++ st ++ ")"
    symb <- right pop
    rs <- findall symb <$> ask
    if rs == []
    then do
        ch <- left pop
        guard $ symb == ch
        tell $ DList.singleton $ "Read " ++ [ch]
    else do
        r <- lift rs
        right $ pushn r
        tell $ DList.singleton $ "Expand " ++ [symb] ++ " -> " ++ r

llsteps :: RWST [(Char, String)] (DList String) (String, String) [] ()
llsteps = do
    llstep
    le <- left isempty
    re <- right isempty
    if (le && re)
    then do
        tell $ DList.singleton $ " (,)"
        tell $ DList.singleton $ "Success!"
    else do
        guard $ not re 
        llsteps
        -- else do
        --     (inp, st) <- get
        --     tell $ DList.singleton $ " (" ++ inp ++ ", " ++ st ++ ")"
        --     tell $ DList.singleton $ "Fail :("

ll :: [(Char, String)] -> String -> [[String]]
ll rs s = (DList.toList . snd) <$> evalRWST llsteps rs (s, [fst $ head rs])

ll' :: [(Char, String)] -> String -> IO ()
ll' rs s = case ll rs s of
    []   -> putStrLn "Fail :("
    reds -> putStrLn $ intercalate "\n---\n" $ unlines <$> reds

lang = [
    ('S', "aSb"),
    ('S', "")
    ]

main :: IO ()
main = do
    lns <- lines <$> getContents
    let (gram, strs) = break (==[]) lns
    let lang = mklang $ unlines gram
    forM_ (tail strs) $ \l -> do
        putStrLn "\n---"
        ll' lang (filter (not . isSpace) l)