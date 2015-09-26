

#ifndef I_GUARD_APPENDFUNCTION_H
#define I_GUARD_APPENDFUNCTION_H

#include <stdio.h>
#include <TSystem.h>
#include <TSysEvtHandler.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>

extern "C" {
  int SetRangeUser(TAxis *axis, double min, double max);
  int hSetGrid(TCanvas *canvas);
  int cModified(TCanvas *canvas);
  int cUpdate(TCanvas *canvas);
  int SetRangeTH(TH1 *hist, double xmin, double xmax, double ymin, double ymax);
  int SetPadMargin(double lmargin, double rmargin, double tmargin, double bmargin);
  int SetXAxisDateTGraph(TGraph *gra, int unixtime);
  int SetXAxisDateTH2D(TH2D *th, int unixtime);
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

