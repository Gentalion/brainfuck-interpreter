module Lib where

import Globals
import Data.Char

data Tape a = Tape [a] a [a]
instance (Show a) => Show (Tape a) where
    show (Tape l c r) = (show $ reverse l) ++ " " ++ show c ++ " " ++ show r
data Command
    = MoveLeft
    | MoveRight
    | Increment
    | Decrement
    | ReadSymbol
    | WriteSymbol
    | Loop [Command]
    deriving (Show)
type Program = [Command]
data ParseError = UnknownSymbol | UnexpectedCloseBracket | NoCloseBracket
    deriving (Show)

parseAny :: String -> Program -> Either ParseError Program
parseAny [] p = Right $ reverse p
parseAny ('[':st) p = 
    case commandOrError of
        Left e  -> Left e
        Right c -> parseAny newSt (c:p) 
    where (newSt, commandOrError) = parseLoop st []
parseAny (sh:st) p = 
    case commandOrError of
        Left UnknownSymbol -> if ignoreUnknownSymbol
                   then parseAny st p
                   else Left UnknownSymbol
        Left e  -> Left e
        Right c -> parseAny st (c:p)
    where commandOrError = parseSimple sh

parseSimple :: Char -> Either ParseError Command
parseSimple c =
    case c of
        '<' -> Right MoveLeft
        '>' -> Right MoveRight
        '+' -> Right Increment
        '-' -> Right Decrement
        '.' -> Right WriteSymbol
        ',' -> Right ReadSymbol
        ']' -> Left UnexpectedCloseBracket
        _   -> Left UnknownSymbol

parseLoop :: String -> [Command] -> (String, Either ParseError Command)
parseLoop [] _ = ([], Left NoCloseBracket)
parseLoop ('[':st) p = 
    case commandOrError of
        Left e  -> (st, Left e)
        Right c -> parseLoop newSt (c:p) 
    where (newSt, commandOrError) = parseLoop st []
parseLoop (']':st) p = (st, Right (Loop $ reverse p))
parseLoop (sh:st) p = 
    case commandOrError of
        Left e  -> if ignoreUnknownSymbol
                   then parseLoop st p
                   else (st, Left e)
        Right c -> parseLoop st (c:p)
    where commandOrError = parseSimple sh

moveLeft :: Tape Int -> Tape Int
moveLeft (Tape [] c r) = Tape [] 0 (c:r)
moveLeft (Tape (lh:lt) c r) = Tape lt lh (c:r)

moveRight :: Tape Int -> Tape Int
moveRight (Tape l c []) = Tape (c:l) 0 []
moveRight (Tape l c (rh:rt)) = Tape (c:l) rh rt

increment :: Tape Int -> Tape Int
increment (Tape l c r) = Tape l (c+1) r

decrement :: Tape Int -> Tape Int
decrement (Tape l c r) = Tape l (c-1) r

writeSymbol :: Tape Int -> IO (Tape Int)
writeSymbol (Tape l c r) = do
    putStr $ (chr c):[]
    return (Tape l c r)

readSymbol :: Tape Int -> IO (Tape Int)
readSymbol (Tape l _ r) = do 
    c <- getChar
    return (Tape l (ord c) r)

executeSimple :: Command -> Tape Int -> IO (Tape Int)
executeSimple c t = do
    if debugPrintTape
        then putStrLn $ show t
        else pure ()
    case c of
        MoveLeft    -> return $ moveLeft t
        MoveRight   -> return $ moveRight t
        Increment   -> return $ increment t
        Decrement   -> return $ decrement t
        ReadSymbol  -> readSymbol t
        WriteSymbol -> writeSymbol t

execute :: Program -> Tape Int -> IO (Tape Int)
execute [] t = return t
execute ((Loop lp):pt) (Tape l 0 r) = execute pt (Tape l 0 r)
execute ((Loop lp):pt) t = do
    newTape <- execute lp t
    case newTape of
        Tape l 0 r -> execute pt newTape
        _          -> execute ((Loop lp):pt) newTape
execute (ph:pt) t = do
    newTape <- executeSimple ph t
    execute pt newTape
