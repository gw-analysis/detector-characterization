/******************************************
 *     File Name: SignalHandlerROOT.cc
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/08/12 16:27:56
 ******************************************/

#include "SignalHandlerROOT.h"

using namespace std;

extern "C" {
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
