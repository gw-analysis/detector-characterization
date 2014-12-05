{-******************************************
  *     File Name: StudentRayleighFunctions.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/05 15:38:36
  *******************************************-}

-- Reference
---- [1] C.Rover, Phys. Rev. D 84, 122004 (2011)
---- [2] ``GSL Reference Manual'', Edition 1.16 

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
-- Student-RayleighのProbability Density Function: [1] Eq.(A18)
---- param1: sigma
---- param2: 自由度 nu
---- param3: 確率変数 x
hkalRanStudentRayleighPdf :: Double -> Double -> Double -> Double
hkalRanStudentRayleighPdf sigma nu x = (*) aX $ RND.gslRanFdistPdf pX 2.0 nu
  where aX = x / sigma / sigma
        pX = x * x / 2.0 / sigma / sigma

-- Student-RayleighのCumulative Distribution Function(下側): [1] Eq.(A19)
---- param1: sigma
---- param2: 自由度 nu
---- param3: 確率変数 x
hkalCdfStudentRayleighP :: Double -> Double -> Double -> Double
hkalCdfStudentRayleighP sigma nu x = hkalCdfFdistP pX nu --RND.gslCdfFdistP pX 2.0 nu
  where pX = x * x / 2.0 / sigma / sigma

-- Student-RayleighのCumulative Distribution Function(上側)
---- param1: sigma
---- param2: 自由度 nu
---- param3: 確率変数 x
hkalCdfStudentRayleighQ :: Double -> Double -> Double -> Double
hkalCdfStudentRayleighQ sigma nu x = 1.0 - hkalCdfStudentRayleighP sigma nu x

-- Student-RayleighのQuantile Function(下側): [1] Eq.(A20)
---- param1: sigma
---- param2: 自由度 nu
---- param3: 累積確率 p
hkalCdfStudentRayleighPinv :: Double -> Double -> Double -> Double
hkalCdfStudentRayleighPinv sigma nu p = sqrt $ norm * qOfFdist
  where norm = 2.0 * sigma *sigma
        qOfFdist = hkalCdfFdistPinv p nu --RND.gslCdfFdistPinv p 2.0 nu

-- Student-RayleighのQuantile Function(上側)
---- param1: sigma
---- param2: 自由度 nu
---- param3: 累積確率 p
hkalCdfStudentRayleighQinv :: Double -> Double -> Double -> Double
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
