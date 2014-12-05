/******************************************
 *     File Name: AppendFunction.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/12/06 00:19:22
 ******************************************/

#ifndef I_GUARD_APPENDFUNCTION_H
#define I_GUARD_APPENDFUNCTION_H

#include <stdio.h>
#include <TSystem.h>
#include <TSysEvtHandler.h>
#include <TCanvas.h>
#include <TH1.h>

extern "C" {
  int SetRangeUser(TAxis *axis, double min, double max);
  int hSetGrid(TCanvas *canvas);
  int cModified(TCanvas *canvas);
  int cUpdate(TCanvas *canvas);
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

