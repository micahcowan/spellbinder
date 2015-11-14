module SpellBookParser
  (
    makeModule
  , parse
  , ParseError(..)
  , ParseErrorType(..)
  )
where

import Data.Char (isSpace)
import Data.List (intercalate)

import Spellbinder.Gestures

type SpellBookEntry = ([ HandUsage ], String)

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

makeModule :: String -> String -> String -> Either ParseError String
makeModule input moduleName spellBookName =
    let result = parse input
     in case result of
        Left err -> Left err
        Right entries -> Right $ "\
\module " ++ moduleName ++ "\n\
\  (\n\
\    " ++ spellBookName ++ "\n\
\  ) in\n\
\\n\
\\&" ++ spellBookName ++ " = \n\
\[\n\
\    "  ++ intercalate "\n  , " (map show entries) ++ "\n\
\]\n"


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
                            else Right (hands, rest') -- good!
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
