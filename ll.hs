{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.Trans
import Control.Monad.RWS.Lazy
import Data.Char
import Data.DList (DList)
import qualified Data.DList as DList
import Data.List

pop :: (MonadState [a] m, MonadFail m) => m a
pop = do
    x:xs <- get
    put xs
    pure x

push :: MonadState [a] m => a -> m ()
push x = state $ \xs -> ((), x:xs)

pushn :: MonadState [a] m => [a] -> m ()
pushn ys = state $ \xs -> ((), ys ++ xs)

peek :: (MonadState [a] m, MonadFail m) => m a
peek = do
    x <- pop
    push x
    pure x

isempty :: MonadState [a] m => m Bool
isempty = state $ \case
    x:xs -> (False, x:xs)
    []   -> (True, [])

trypop :: (Eq a, MonadState [a] m, MonadFail m) => a -> m Bool
trypop x = do
    e <- isempty
    if e
    then pure False
    else do
        y <- peek
        if x == y
        then pop >> pure True
        else pure False

left :: (Monoid w, Monad m) => RWST r w q m a -> RWST r w (q, s) m a
left s = RWST $ \w (l, r) -> do
    (a, l', w) <- runRWST s w l
    pure (a, (l', r), w)

right :: (Monoid w, Monad m) => RWST r w s m a -> RWST r w (q, s) m a
right s = RWST $ \w (l, r) -> do
    (a, r', w) <- runRWST s w r
    pure (a, (l, r'), w)

findall :: Eq a => a -> [(a, b)] -> [b]
findall _ []                   = []
findall x ((k, v):xs) | x == k    = v : findall x xs
                      | otherwise = findall x xs

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

main :: IO ()
main = do
    lns <- lines <$> getContents
    let (gram, strs) = break (==[]) lns
    let lang = mklang $ unlines gram
    forM_ (tail strs) $ ll' lang