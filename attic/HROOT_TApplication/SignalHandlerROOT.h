/******************************************
 *     File Name: SignalHandlerROOT.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/08/09 01:47:31
 ******************************************/

#ifndef I_GUARD_SIGNALHANDLERROOT_H
#define I_GUARD_SIGNALHANDLERROOT_H

#include <stdio.h>
#include <TSystem.h>
#include <TSysEvtHandler.h>

class MySignalHandler : public TSignalHandler{
 public:
 MySignalHandler(ESignals sig) : TSignalHandler(sig){}
  Bool_t Notify();
};

#endif /*  I_GUARD_SIGNALHANDLERROOT_H  */

