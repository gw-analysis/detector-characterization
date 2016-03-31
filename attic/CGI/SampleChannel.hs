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
  ,sampleChs
) where


defaultChs = f 3
  where f 0 = frameFull
        f 1 = xEndEnvCh
        f 2 = sampleIOO
        f 3 = sampleChs
        f _ = sampleChs

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

sampleChs = sampleVIS ++ sampleIOO ++ sampleGen ++ sampleMIF 

sampleGen = ["K1:LSC-MICH_ERR_CAL_OUT_DQ"
            ,"K1:GRD-MICH_LOCK_STATE_N"
            ,"K1:GRD-PSL_STATE_N"
            ,"K1:GRD-IMC_LOCK_STATE_N" 
            ,"K1:GRD-IFO_STATE_N"
            ,"K1:GRD-LSC_LOCK_STATE_N"
            ,"K1:GRD-VIS_BS_STATE_N"
            ,"K1:GRD-VIS_MCE_STATE_N"
            ,"K1:GRD-VIS_MCI_STATE_N"
            ,"K1:GRD-VIS_MCO_STATE_N"
            ,"K1:GRD-VIS_PR3_STATE_N"
            ] 

sampleIOO = ["K1:PSL-PMC_TRANS_DC_OUT_DQ"
            ,"K1:PSL-PMC_MIXER_OUT_DQ"
            ,"K1:PSL-PMC_FAST_MON_OUT_DQ"
            ,"K1:PSL-PMC_SLOW_MON_OUT_DQ"
            ,"K1:PSL-PMC_REFL_DC_OUT_DQ"
            ,"K1:IMC-MCL_REFL_DC_OUT_DQ" 
            ,"K1:IMC-MCL_L_OUT_DQ" 
            ,"K1:IMC-MCL_F_OUT_DQ"
            ,"K1:IMC-MCL_SERVO_OUT_DQ"
            ,"K1:IMC-MCL_TRANS_OUT_DQ"
            ,"K1:IMC-MCL_TRANS_OUTPUT"
            ,"K1:IMC-MCL_SERVO_OUTPUT"
            ,"K1:IMC-MCL_DC_LOOP_OUTPUT"
            ]

sampleVIS = ["K1:VIS-MCI_TM_OPLEV_YAW_OUT_DQ"
            ,"K1:VIS-MCE_TM_OPLEV_YAW_OUT_DQ"
            ,"K1:VIS-MCO_TM_OPLEV_YAW_OUT_DQ"
            ,"K1:VIS-MCI_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-MCE_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-MCO_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-BS_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-BS_TM_OPLEV_YAW_OUT_DQ"
            ,"K1:VIS-EX_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-EX_TM_OPLEV_YAW_OUT_DQ"
            ,"K1:VIS-EY_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-EY_TM_OPLEV_YAW_OUT_DQ"
            ,"K1:VIS-PR3_TM_OPLEV_PIT_OUT_DQ"
            ,"K1:VIS-PR3_TM_OPLEV_YAW_OUT_DQ"
            ]

sampleMIF = ["K1:LSC-REFL_PDA1_DC_OUT_DQ"
            ,"K1:LSC-MICH_OUT_DQ"
            ,"K1:LSC-IMT_QPDA1_PIT_OUT_DQ"
            ,"K1:LSC-IMT_QPDA1_YAW_OUT_DQ" 
            ]
