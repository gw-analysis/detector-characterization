

module HasKAL.SpectrumUtils.Signature where

import Numeric.LinearAlgebra


type Spectrogram = (Vector Double, Vector Double, Matrix Double) --(t,f,specgram)
type Spectrum = (Vector Double, Vector Double)
