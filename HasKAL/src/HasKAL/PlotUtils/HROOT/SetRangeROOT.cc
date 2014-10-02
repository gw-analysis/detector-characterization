/******************************************
 *     File Name: SetRangeROOT.cc
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/10/02 17:44:29
 ******************************************/

#include "SetRangeROOT.h"

extern "C" {
  int SetRangeUser(TAxis *axis, double min, double max){
    axis->SetRangeUser(min, max);
    return 0; // ダミーの返り値
  }
}

