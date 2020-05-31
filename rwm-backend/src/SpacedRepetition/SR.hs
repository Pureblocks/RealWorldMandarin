module SpacedRepetition.SR 
    ( interRepetitionInterval
    ) where

-- | This module defines a general abstraction for any spaced repitition technique
-- used within RealWorldMandarin. Applications are Character Recognition module and
-- general study cards.

type Rep = Int

type Interval = Int

-- data Interval = Seconds Int
--               | Minutes Int
--               | Hours Int
--               | Days Int
--               deriving (Show, Eq, Ord)

type EFactor = Int

interRepetitionInterval :: Rep -> EFactor -> Interval
interRepetitionInterval 1 _  = 1
interRepetitionInterval 2 _  = 6
interRepetitionInterval n ef = interRepetitionInterval (n - 1) ef * ef

easinessFactor :: Float -> Float -> Float
easinessFactor previousEf q = previousEf - 0.8 + 0.28 * q - 0.02 * q * q

-- EF':=EF+(0.1-(5-q)*(0.08+(5-q)*0.02))
--EF' - new value of the E-Factor
--EF - old value of the E-Factor
--q - quality of the response
--f - function used in calculating EF'.