{- |
Module      : SampleChannel
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto :mail:
              Akinobu Miyamoto
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2016/01/12 
-}


module SampleChannel (
   defaultChs
  ,frameFull
  ,xEndEnvCh
  ,sampleIOO
) where


defaultChs = f 0
  where f 0 = frameFull
        f 1 = xEndEnvCh
        f 2 = sampleIOO
        f _ = sampleIOO

frameFull = [{-"K1:MCL-F_OUT_DQ"
            ,"K1:MCL-I_OUT_DQ"
            ,"K1:MCL-L_OUT_DQ"
            ,"K1:MCL-MCL_OUT_DQ"
            ,"K1:MCL-REFL_DC_OUT_DQ"
            ,"K1:MCL-TRANS_OUT_DQ"
            ,-}"K1:PSL-FSS_FAST_MON_OUT_DQ"
            ,"K1:PSL-FSS_MIXER_OUT_DQ"
            ,"K1:PSL-FSS_PC_MON_OUT_DQ"
            ,"K1:PSL-FSS_REFL_DC_OUT_DQ"
            ,"K1:PSL-FSS_SLOW_MON_OUT_DQ"
            ,"K1:PSL-FSS_TRANS_DC_OUT_DQ"
            ,"K1:PSL-PMC_FAST_MON_OUT_DQ"
            ,"K1:PSL-PMC_MIXER_OUT_DQ"
            ,"K1:PSL-PMC_REFL_DC_OUT_DQ"
            ,"K1:PSL-PMC_SLOW_MON_OUT_DQ"
            ,"K1:PSL-PMC_TRANS_DC_OUT_DQ"
            ,"K1:VIS-MCE_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-MCE_TM_OPLEV_YAW_OUT_DQ"
            ]

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
