/******************************************
 *     File Name: AppendFunction.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/10/03 14:52:38
 ******************************************/

#ifndef I_GUARD_APPENDFUNCTION_H
#define I_GUARD_APPENDFUNCTION_H

#include <stdio.h>
#include <TSystem.h>
#include <TSysEvtHandler.h>
#include <TH1.h>

extern "C" {
  int SetRangeUser(TAxis *axis, double min, double max);
}

class MySignalHandler : public TSignalHandler{
 public:
 MySignalHandler(ESignals sig) : TSignalHandler(sig){}
  Bool_t Notify();
};

class AddSignalHandle{
 public:
 AddSignalHandle();
};

#endif /*  I_GUARD_APPENDFUNCTION_H  */

