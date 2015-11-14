module Spellbinder.Gestures
  (
    Gesture(..)
  , HandUsage(..)

  , gesture
  , handedGesture
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

gesture :: Char -> Either Char Gesture
gesture 'C' = Right Clap
gesture 'D' = Right Digit
gesture 'F' = Right Finger
gesture 'P' = Right Palm
gesture 'S' = Right Snap
gesture 'W' = Right Wave
gesture c   = Left c

data HandUsage =
    OneHand Gesture
  | BothHands Gesture
    deriving (Show)

handedGesture :: Char -> Either Char HandUsage
handedGesture c =
    case gesture (toUpper c) of
        Right gest ->
            Right $ if isUpper c
                    then
                        OneHand gest
                    else
                        BothHands gest
        Left c -> Left c
