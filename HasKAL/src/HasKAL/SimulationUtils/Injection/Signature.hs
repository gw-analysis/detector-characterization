{-# LANGUAGE TemplateHaskell #-}

module HasKAL.SimulationUtils.Injection.Signature where

import Control.Lens

type SigType = String


data SOURCE_TYPE = SOURCE_TYPE {  _sigType :: SigType
                                , _longitude :: Double
                                , _latitude :: Double
                                , _psi :: Double
                                , _fs :: Double
                               } deriving (Show)
$(makeLenses ''SOURCE_TYPE)
