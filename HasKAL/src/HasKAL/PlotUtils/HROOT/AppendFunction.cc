/******************************************
 *     File Name: AppendFunction.cc
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/10/03 14:51:49
 ******************************************/

#include "AppendFunction.h"

using namespace std;
extern "C" {
  int SetRangeUser(TAxis *axis, double min, double max){
    axis->SetRangeUser(min, max);
    return 0; // ダミーの返り値
  }

  int AddSignalHandle (){
    gSystem->AddSignalHandler(new MySignalHandler(kSigInterrupt) );
    return 0; // Haskell上で必要なダミーの返り値(C++で使う場合はvoidで良い)
  }
}

Bool_t MySignalHandler::Notify(){
  gSystem->ExitLoop();
  fprintf(stderr, " signal catched\n");
  return kTRUE;
}
