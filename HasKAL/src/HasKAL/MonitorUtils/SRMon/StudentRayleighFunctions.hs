{-# HADDOCK Markdown #-}
{- |
Module      : HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

Student Rayleigh Functions

- [1] C.Rover, Phys. Rev. D 84, 122004 (2011)
-}

module HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions (
   hkalRanStudentRayleighPdf
  ,hkalCdfStudentRayleighP
  ,hkalCdfStudentRayleighQ
  ,hkalCdfStudentRayleighPinv
  ,hkalCdfStudentRayleighQinv
) where

import Numeric.GSL.Special.Gamma (gamma)
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND

{--  External Functions  --}
-- | \[1\] Eq.(A18)
hkalRanStudentRayleighPdf :: Double -- ^ sigma
                          -> Double -- ^ nu
                          -> Double -- ^ x
                          -> Double -- ^ p(x)
hkalRanStudentRayleighPdf sigma nu x = (*) aX $ RND.gslRanFdistPdf pX 2.0 nu
  where aX = x / sigma / sigma
        pX = x * x / 2.0 / sigma / sigma

-- | \[1\] Eq.(A19)
hkalCdfStudentRayleighP :: Double -- ^ sigma
                        -> Double -- ^ nu
                        -> Double -- ^ x
                        -> Double -- ^ P(x)
hkalCdfStudentRayleighP sigma nu x = hkalCdfFdistP pX nu --RND.gslCdfFdistP pX 2.0 nu
  where pX = x * x / 2.0 / sigma / sigma

hkalCdfStudentRayleighQ :: Double -- ^ sigma
                        -> Double -- ^ nu
                        -> Double -- ^ x
                        -> Double -- ^ Q(x)
hkalCdfStudentRayleighQ sigma nu x = 1.0 - hkalCdfStudentRayleighP sigma nu x

-- | \[1\] Eq.(A20)
hkalCdfStudentRayleighPinv :: Double -- ^ sigma
                           -> Double -- ^ nu
                           -> Double -- ^ P(x)
                           -> Double -- ^ x
hkalCdfStudentRayleighPinv sigma nu p = sqrt $ norm * qOfFdist
  where norm = 2.0 * sigma *sigma
        qOfFdist = hkalCdfFdistPinv p nu --RND.gslCdfFdistPinv p 2.0 nu

hkalCdfStudentRayleighQinv :: Double -- ^ sigma
                           -> Double -- ^ nu
                           -> Double -- ^ Q(x)
                           -> Double -- ^ x
hkalCdfStudentRayleighQinv sigma nu p = hkalCdfStudentRayleighPinv sigma nu (1.0 - p)


{--  Internal Functions  --}
-- Cumulative functions of F-dist in GSL maybe contain bugs
---- CDF of F-dist (nu1 = 2.0): [2] p.234
hkalCdfFdistP :: Double -> Double -> Double
hkalCdfFdistP x nu = (gamma (1.0+nu')) * (nu**nu') * (nu**(-nu') - (nu+2.0*x)**(-nu')) / ((gamma nu') * nu')
  where nu' = 0.5 * nu

---- Quantile of F-dist (nu1 = 2.0): [2] p.234
hkalCdfFdistPinv :: Double -> Double -> Double
hkalCdfFdistPinv p nu = nu' * ( (1.0 - p*nu'*(gamma nu') / (gamma (1.0+nu')) )**(-1.0/nu') - 1.0 )
  where nu' = 0.5 * nu
