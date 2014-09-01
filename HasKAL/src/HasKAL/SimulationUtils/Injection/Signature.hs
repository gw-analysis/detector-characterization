
module HasKAL.SimulationUtils.Injection.Signature where


type SigType = String


data SOURCE_TYPE = SOURCE_TYPE {  sigType :: SigType
                                , longitude :: Double
                                , latitude :: Double
                                , psi :: Double
                                , fs :: Double
                               } deriving (Show)
