/******************************************
 *     File Name: SignalHandlerROOT.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/08/11 11:18:39
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

class AddSignalHandle{
 public:
 AddSignalHandle();
};

#endif /*  I_GUARD_SIGNALHANDLERROOT_H  */

