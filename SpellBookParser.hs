module SpellBookParser
  (
    Gesture(..)
  , RequiredHands(..)

  , gesture
  , handedGesture

  , parse
  )
where

import Data.Char (toUpper, isUpper, isSpace)

-- import Control.Monad.Either -- GHC-only? Needed to treat Either as a monad

data Gesture =
    Clap
  | Digit
  | Finger
  | Palm
  | Snap
  | Wave
  | Stab
  | NoGesture
    deriving (Show, Eq)

gesture :: Char -> Either Char Gesture
gesture 'C' = Right Clap
gesture 'D' = Right Digit
gesture 'F' = Right Finger
gesture 'P' = Right Palm
gesture 'S' = Right Snap
gesture 'W' = Right Wave
gesture c   = Left c

data RequiredHands =
    OneHand Gesture
  | BothHands Gesture
    deriving (Show)

handedGesture :: Char -> Either Char RequiredHands
handedGesture c =
    case gesture (toUpper c) of
        Right gest ->
            Right $ if isUpper c
                    then
                        OneHand gest
                    else
                        BothHands gest
        Left c -> Left c

type SpellBookEntry = ([ RequiredHands ], String)

data ParseError =
    ParseError {
        errType :: ParseErrorType
      , line    :: Int
      , column  :: Int
      , char    :: Char
    }
    deriving (Show)

data ParseErrorType =
    NonGestureChar
  | NoGesturesGiven
  | NoSpellGiven
    deriving (Show)

errNonGestureChar = ParseError NonGestureChar
errNoGesturesGiven = ParseError NoGesturesGiven
errNoSpellGiven = ParseError NoSpellGiven

parse :: String -> Either ParseError [ SpellBookEntry ]
parse = mapM id . map parseLine . zip [1..] . lines

-- 
parseLine :: (Int, String) -> Either ParseError SpellBookEntry
parseLine (lnum, line) = 
    case parseHands ([], line) of
        (hands, rest) ->
          let col = length hands + 1 in
            case rest of
                [ ] ->
                    if null hands
                    then Left $ errNoGesturesGiven lnum col '\NUL'
                    else Left $ errNoSpellGiven    lnum col '\NUL'
                (c:cs) ->
                    if isSpace c
                    then
                        if null hands
                        then
                            Left $ errNoGesturesGiven lnum col '\NUL'
                        else
                          let rest' = dropWhile isSpace rest in
                            if null rest'
                            then Left $ errNoSpellGiven lnum col '\NUL'
                            else Right (reverse hands, rest') -- good!
                    else
                        Left $ errNonGestureChar lnum col c

-- parse the hand gestures out of the line, leaving the rest
-- (starting with first non-gesture char)
parseHands :: SpellBookEntry -> SpellBookEntry
parseHands (hands, line) =
    case line of
        [ ] ->
            (hands, line)
        (c:cs) ->
            case handedGesture c of
                Right g -> parseHands ((g : hands), cs)
                Left c -> (hands, line)
