module IMBH
(
  calcEta
, calcTotalmass
, rhoImbh
, rhodistImbh
, distImbh
)
where


{-

// Description

calcEta
input:
mass1 : mass of a star [unit of solar mass]
mass2 : mass of a star [unit of solar mass]
output:
symmetric mass ratio

calcTotalmass
input:
mass1 : mass of a star [unit of solar mass]
mass2 : mass of a star [unit of solar mass]
ooutput:
total mass of the binary sistem

rhoImbh
calculation of a signal-to-noise ratio of IMBH with a given binary mass, distance, detector spectrum data.
input :
mass1 : mass of a star [unit of solar mass]
mass2 : mass of a star [unit of solar mass]
distance : distance from the earth [unit of Mega parsec]
spectrumData : [(frequency, powerspectrum)]
output:
signal-to-noise raio

rhodistImbh
calculation of a SNR*distance of IMBH with a given binary mass,  detector spectrum data.
input :
mass1 : mass of a star [unit of solar mass]
mass2 : mass of a star [unit of solar mass]
spectrumData : [(frequency,  powerspectrum)]
output:
SNR*distance
where
SNR : signal-to-noise raio
distance : distance from the earth [unit of Mega parsec]


distImbh
calculation of a distance of IMBH with a given binary mass, SNR=8, detector spectrum data.
input :
mass1 : mass of a star [unit of solar mass]
mass2 : mass of a star [unit of solar mass]
spectrumData : [(frequency, powerspectrum)]
output:
distance [unit of Mpc]



// Test code for IMBHMon

import IMBH

main :: IO Double
main = do
    contents <- readFile "prebKAGRA.dat"
    let spectrumData = map ((\[x, y] -> (read x::Double, read y**2::Double)) . words) (lines contents)

    return $ distImbh 100 100 spectrumData


-}

-- physical cpnstant
msolar_time :: Double
msolar_time = 4.92579497077314E-6
parsec_sec :: Double
parsec_sec  = 1.0292712503E8
mparsec_sec :: Double
mparsec_sec = parsec_sec*1E6


data ParamType = F_MERGE | F_RING| SIGMA | F_CUT deriving (Eq)


calcEta :: Double -> Double -> Double
calcEta mass1 mass2 = mass1*mass2/(calcTotalmass mass1 mass2)**2

calcTotalmass :: Double -> Double -> Double
calcTotalmass mass1 mass2 = mass1 + mass2

getWavParam :: (Double, Double, Double) -> Double -> Double -> Double
getWavParam (a, b, c) var_eta var_tmass = (a*var_eta**2 + b*var_eta + c)/(pi*var_tmass)

setParam :: ParamType -> (Double, Double, Double)
setParam ptype
  | ptype == F_MERGE = (2.6740 * 1E-1, 4.4810 * 1E-2, 9.5560 * 1E-2)
  | ptype == F_RING  = (5.9411 * 1E-1, 8.9794 * 1E-2, 1.9111 * 1E-1)
  | ptype == SIGMA   = (5.0801 * 1E-1, 7.7515 * 1E-2, 2.2369 * 1E-2)
  | ptype == F_CUT   = (8.4845 * 1E-1, 1.2848 * 1E-1, 2.7299 * 1E-1)

getF_merge :: Double -> Double -> Double
getF_merge = getWavParam (setParam F_MERGE)
getF_ring :: Double -> Double -> Double
getF_ring  = getWavParam (setParam F_RING)
getSigma :: Double -> Double -> Double
getSigma   = getWavParam (setParam SIGMA)
getF_cut :: Double -> Double -> Double
getF_cut   = getWavParam (setParam F_CUT)

calcRhoCoef :: Double -> Double -> Double-> Double-> Double
calcRhoCoef var_tmass var_fmerg var_eta dist = var_tmass**(5/6)*var_fmerg**(-7/6)/dist/pi**(2/3)*(5*var_eta/6)**(1/2)

calcRhoDistCoef :: Double -> Double -> Double -> Double
calcRhoDistCoef var_tmass var_fmerg var_eta = var_tmass**(5/6)*var_fmerg**(-7/6)/pi**(2/3)*(5*var_eta/6)**(1/2)

calcRhoInsp :: Double -> Double -> [(Double, Double)] -> Double
calcRhoInsp var_flow var_fmerg spectrumData = sum $ zipWith (*) shList df
  where shList = [(f/var_fmerg)**(-7/3)/sh | (f, sh)<-spectrumData,  f>var_flow, f<var_fmerg]
        fList = [f | (f, _)<-spectrumData,  f>var_flow,  f<var_fmerg]
        df = zipWith (-) (tail fList) (init fList)

calcRhoMerg :: Double -> Double -> [(Double, Double)] -> Double
calcRhoMerg var_fmerg var_fring spectrumData = sum $ zipWith (*) shList df
  where shList = [(f/var_fmerg)**(-4/3)/sh | (f, sh)<-spectrumData,  f>var_fmerg, f<var_fring]
        fList = [f | (f, _)<-spectrumData,  f>var_fmerg,  f<var_fring]
        df = zipWith (-) (tail fList) (init fList)

calcRhoRing :: Double -> Double -> Double -> Double -> [(Double, Double)] -> Double
calcRhoRing var_fmerg var_fring var_fcut sigma spectrumData = w**2 * sum (zipWith (*) shList df)
  where w = pi*sigma/2*(var_fring/var_fmerg)**(-2/3)
        fList = [f | (f, _)<-spectrumData, f>var_fring, f<var_fcut]
        shList = [lorentz f**2/sh | (f, sh)<-spectrumData, f>var_fmerg, f<var_fring]
         where lorentz f = 1/(2*pi)*sigma/((f-var_fring)**2+sigma**2/4)
        df = zipWith (-) (tail fList) (init fList)



rhoImbhCore :: Double -> Double -> Double -> Double -> [(Double, Double)]-> Double
rhoImbhCore flow mass1 mass2 distMPC spectrumData = do
  let mass1ST = mass1*msolar_time
      mass2ST = mass2*msolar_time
      eta   = calcEta mass1ST mass2ST
      tmass = calcTotalmass mass1ST mass2ST
      fmerg = getF_merge eta tmass
      fring = getF_ring eta tmass
      fcut  = getF_cut eta tmass
      sigma = getSigma eta tmass
      dist = distMPC*mparsec_sec

      rhoCoef = calcRhoCoef tmass fmerg eta dist
      rhoInsp = calcRhoInsp flow fmerg spectrumData
      rhoMerg = calcRhoMerg fmerg fring spectrumData
      rhoRing = calcRhoRing fmerg fring fcut sigma spectrumData

  rhoCoef * (rhoCoef + rhoInsp + rhoMerg + rhoRing)**(1/2)

rhoImbh :: Double -> Double -> Double -> [(Double,  Double)]-> Double
rhoImbh = rhoImbhCore 5

rhodistImbhCore :: Double -> Double -> Double -> [(Double, Double)]-> Double
rhodistImbhCore flow mass1 mass2 spectrumData = do
  let mass1ST = mass1*msolar_time
      mass2ST = mass2*msolar_time
      eta   = calcEta mass1ST mass2ST
      tmass = calcTotalmass mass1ST mass2ST
      fmerg = getF_merge eta tmass
      fring = getF_ring eta tmass
      fcut  = getF_cut eta tmass
      sigma = getSigma eta tmass

      rhodistCoef = calcRhoDistCoef tmass fmerg eta
      rhoInsp = calcRhoInsp flow fmerg spectrumData
      rhoMerg = calcRhoMerg fmerg fring spectrumData
      rhoRing = calcRhoRing fmerg fring fcut sigma spectrumData

  rhodistCoef * (rhoInsp + rhoMerg + rhoRing)**(1/2)

rhodistImbh :: Double -> Double -> [(Double,  Double)]-> Double
rhodistImbh = rhodistImbhCore 5

distImbhCore :: Double -> Double -> Double -> [(Double, Double)]-> Double
distImbhCore flow mass1 mass2 spectrumData = do
  let mass1ST = mass1*msolar_time
      mass2ST = mass2*msolar_time
      eta   = calcEta mass1ST mass2ST
      tmass = calcTotalmass mass1ST mass2ST
      fmerg = getF_merge eta tmass
      fring = getF_ring eta tmass
      fcut  = getF_cut eta tmass
      sigma = getSigma eta tmass

      rhodistCoef = calcRhoDistCoef tmass fmerg eta
      rhoInsp = calcRhoInsp flow fmerg spectrumData
      rhoMerg = calcRhoMerg fmerg fring spectrumData
      rhoRing = calcRhoRing fmerg fring fcut sigma spectrumData

  rhodistCoef * (rhoInsp + rhoMerg + rhoRing)**(1/2) /8.0 /mparsec_sec

distImbh :: Double -> Double -> [(Double,  Double)]-> Double
distImbh = distImbhCore 5





