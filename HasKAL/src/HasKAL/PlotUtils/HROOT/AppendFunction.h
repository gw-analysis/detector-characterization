

#ifndef I_GUARD_APPENDFUNCTION_H
#define I_GUARD_APPENDFUNCTION_H

#include <stdio.h>
#include <TStyle.h>
#include <TSystem.h>
#include <TSysEvtHandler.h>
#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>

extern "C" {
  void SetRangeUser(TAxis *axis, double min, double max);
  void hSetGrid(TCanvas *canvas);
  void cModified(TCanvas *canvas);
  void cUpdate(TCanvas *canvas);
  void SetRangeTH(TH1 *hist, double xmin, double xmax, double ymin, double ymax);
  void SetPadMargin(double lmargin, double rmargin, double tmargin, double bmargin);
  void SetXAxisDateTGraph(TGraph *gra, int unixtime);
  void SetXAxisDateTH2D(TH2D *th, int unixtime);
  void SetPallete(int color);
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

