/******************************************
 *     File Name: AppendFunction.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2015/07/16 11:48:39
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
  int SetRangeTH(TH1 *hist, double xmin, double xmax, double ymin, double ymax);
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

