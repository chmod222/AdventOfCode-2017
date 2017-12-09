module Main (main) where

import Control.Monad

import Data.Maybe
import Data.List

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

{-
 - Generic parser combinator stuff
 -}
instance Functor Parser where
    fmap f p = Parser $ \s -> [(f x, y) | (x, y) <- runParser p s]

instance Applicative Parser where
    pure t = Parser $ \s -> [(t, s)]
    m <*> k = Parser $
        \s -> [(f x, y) | (f, v) <- runParser m s
                        , (x, y) <- runParser k v]
    
instance Monad Parser where
    return = pure
    m >>= k = Parser $
        \s -> [(x, y) | (u, v) <- runParser m s
                      , (x, y) <- runParser (k u) v]

zero :: Parser a
zero = Parser $ \s -> []

unit :: Parser Char
unit = Parser unit'
  where
    unit' [] = []
    unit' (x:xs) = [(x, xs)]

many :: Parser a -> Parser [a]
many p = choice (repeat) (return [])
  where
    repeat = do
        r <- p
        rs <- many p

        return $ r : rs

choice :: Parser a -> Parser a -> Parser a
choice a b = Parser $ \s ->
    if null $ runParser a s
        then runParser b s
        else runParser a s

anyP :: [Parser a] -> Parser a
anyP = foldl choice zero

optional :: Parser a -> Parser (Maybe a)
optional p = choice (fmap Just p) (return Nothing)

filt :: Parser a -> (a -> Bool) -> Parser a
filt a pred = do
    a' <- a
    if pred a'
        then return a'
        else zero

literal :: Char -> Parser Char
literal c = filt unit (\c' -> c' == c)

notLiteral :: Char -> Parser Char
notLiteral c = filt unit (\c' -> c' /= c)

{-
 - Group parsing
 -}
data Chunk = Group [Chunk]
           | Garbage String
    deriving (Show)

parseChunk :: Parser Chunk
parseChunk = choice parseGroup parseGarbage

groupContents :: Parser [Chunk]
groupContents = do
    c <- parseChunk

    choice (do literal ','
               cs <- groupContents 

               return $ c : cs)

           (return $ c : [])

parseGroup :: Parser Chunk
parseGroup = do
    literal '{'
    content <- optional groupContents
    literal '}'

    return . Group $ concat . maybeToList $ content

parseGarbage :: Parser Chunk
parseGarbage = do
    literal '<'
    garbage <- many
        (anyP [ (literal '!' >> unit >> return "")
              , (notLiteral '>' >>= return . (:[]))])

    literal '>'

    return $ Garbage (concat garbage)

renderChunk :: Chunk -> String
renderChunk (Group cs) = "{" ++ (intercalate "," . map renderChunk $ cs) ++ "}"
renderChunk (Garbage x) = "<...>"

groupScore :: Chunk -> Int -> Int
groupScore (Garbage _) lv = 0
groupScore (Group gs) lv = lv + (sum $ map (\c -> groupScore c (succ lv)) gs)

garbageScore :: Chunk -> Int
garbageScore (Garbage g) = length g
garbageScore (Group gs) = sum $ map garbageScore gs


main :: IO ()
main = do
    inputLines <- fmap lines getContents

    let parsedLines = map (runParser parseChunk) inputLines

    forM_ parsedLines (\l -> do
        let group = fst . head $ l
        let gscore = groupScore group 1
        let gbscore = garbageScore group

        putStrLn $ renderChunk group
        putStrLn $ "  Score: " ++ (show gscore)
        putStrLn $ "  Garbage score: " ++ (show gbscore))
