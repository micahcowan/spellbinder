module SpellBookParser
  (
    makeModule
  , parse
  )
where

import Data.Char (isSpace, toLower, toUpper)
import Data.List (intercalate)

import Spellbinder.Gestures

type SpellBookEntry = ([ HandUsage ], String)

makeModule :: String -> String -> String -> String
makeModule moduleName spellBookName input =
    let entries = parse input
     in "\
\module " ++ moduleName ++ "\n\
\  (\n\
\    " ++ spellBookName ++ "\n\
\  ) in\n\
\\n\
\\&" ++ spellBookName ++ " = \n\
\[\n\
\    "  ++ intercalate "\n  , " (map show entries) ++ "\n\
\]\n"


parse :: String -> [SpellBookEntry]
parse = map parseLine . zip [1..] . lines

parseLine :: (Int, String) -> SpellBookEntry
parseLine (lnum, line) = 
    let (handSpec, spellName') = span (not . isSpace) line
        spellName = concatMap capitalize . words $ dropWhile isSpace spellName'
        capitalize = zipWith ($) (toUpper : repeat toLower)
    in
        case parseSpellHands handSpec of
            Left bad -> error $
                            "Bad gesture " ++ show bad
                            ++ " in " ++ handSpec ++ ", line " ++ show lnum
            Right hands -> (reverse hands, spellName)
