module Spellbinder.Gestures
  (
    Gesture(..)
  , HandUsage(..)

  , gesture
  , handedGesture
  , parseSpellHands
  )
where

import Data.Char (toUpper, isUpper)

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

gestureList =
    [
        ('C', Clap)
      , ('D', Digit)
      , ('F', Finger)
      , ('P', Palm)
      , ('S', Snap)
      , ('W', Wave)
      , ('>', Stab)
      , ('-', NoGesture)
    ]
gesture :: Char -> Maybe Gesture
gesture = flip lookup gestureList

data HandUsage =
    OneHand Gesture
  | BothHands Gesture
    deriving (Show)

-- For a non-valid gesture (upper or lower) "c", returns Left c
-- Otherwise, Right HandUsage
handedGesture :: Char -> Either Char HandUsage
handedGesture c =
    case gesture (toUpper c) of
        Just gest ->
            Right $ if isUpper c
                    then
                        OneHand gest
                    else
                        BothHands gest
        Nothing -> Left c

parseSpellHands :: String -> Either (Int, Char) [ HandUsage ]
parseSpellHands str =
    let indexedHands = zip [1..] $ map handedGesture str
        liftOut (n, either) = case either of
            Left c -> Left (n, c)
            Right hand -> Right (n, hand)
    in
        case mapM liftOut indexedHands of
            Left err -> Left err
            Right hs -> Right $ map snd hs
