{- |
Module      : SampleChannel
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/11/18 20:33:27
-}


module SampleChannel (
   xEndEnvCh
  ,sampleIOO
) where

xEndEnvCh = ["K1:PEM-EX_ACC_NO2_X_FLOOR"
            ,"K1:PEM-EX_ACC_NO2_Y_FLOOR"
            ,"K1:PEM-EX_ACC_NO2_Z_FLOOR"
            ,"K1:PEM-EX_MAG_X_FLOOR"
            ,"K1:PEM-EX_MAG_Y_FLOOR"
            ,"K1:PEM-EX_MAG_Z_FLOOR"
            ,"K1:PEM-EX_MIC_FLOOR"
            ,"K1:PEM-EX_REF"
            ]

sampleIOO = ["K1:PSL-PMC_TRANS_DC_OUT_DQ"
            ,"K1:PSL-PMC_MIXER_OUT_DQ"
            ,"K1:PSL-PMC_FAST_MON_OUT_DQ"
            ,"K1:PSL-PMC_SLOW_MON_OUT_DQ"
            ,"K1:PSL-PMC_REFL_DC_OUT_DQ"
            ]
