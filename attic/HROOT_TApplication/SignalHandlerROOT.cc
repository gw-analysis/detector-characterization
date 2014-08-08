/******************************************
 *     File Name: SignalHandlerROOT.cc
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/08/09 01:53:46
 ******************************************/

#include "MySignalHandler.h"

using namespace std;

Bool_t MySignalHandler::Notify(){
  gSystem->ExitLoop();
  fprintf(stderr, " signal catched\n");
  return kTRUE;
}

/*
Usage: gSystem->AddSignalHandler(new MySignalHandler(kSigInterrupt) );
*/

