/******************************************
 *     File Name: SignalHandlerROOT.cc
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/08/11 11:36:51
 ******************************************/

#include "SignalHandlerROOT.h"

using namespace std;

extern "C" {
  void AddSignalHandle (){
    gSystem->AddSignalHandler(new MySignalHandler(kSigInterrupt) );
    return;
  }
}

Bool_t MySignalHandler::Notify(){
  gSystem->ExitLoop();
  fprintf(stderr, " signal catched\n");
  return kTRUE;
}
