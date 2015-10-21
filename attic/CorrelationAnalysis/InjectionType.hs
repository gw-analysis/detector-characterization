--module HasKAL.simulationUtils.injectionType(
module InjectionType(
       InjectionType(ExponentialSine, Sine)
    ) where

data InjectionType = ExponentialSine | Sine deriving (Eq, Read)
