

#include "AppendFunction.h"

using namespace std;

extern "C" {
  void SetRangeUser(TAxis *axis, double min, double max){
    axis->SetRangeUser(min, max);
    return;
  }

  void hSetGrid (TCanvas *canvas){
    canvas->SetGrid();
    return;
  }

  void cModified(TCanvas *canvas){
    canvas->Modified();
    return;
  }

  void cUpdate(TCanvas *canvas){
    canvas->Update();
    return;
  }

  int AddSignalHandle (){
    gSystem->AddSignalHandler(new MySignalHandler(kSigInterrupt) );
    return 0;
  }

  void SetRangeTH(TH1 *hist, double xmin, double xmax, double ymin, double ymax){
    if (xmin < xmax) hist->GetXaxis()->SetRangeUser(xmin, xmax);
    if (ymin < ymax) hist->GetYaxis()->SetRangeUser(ymin, ymax);
    return;
  }

  void SetPadMargin(double lmargin, double rmargin, double tmargin, double bmargin){
    gPad->SetLeftMargin(lmargin);
    gPad->SetRightMargin(rmargin);
    gPad->SetTopMargin(tmargin);
    gPad->SetBottomMargin(bmargin);
    return;
  }

  void SetXAxisDateTGraph(TGraph *gra, int unixtime){
    int offset = unixtime - 788918400;// rootの基準は1995/1/1 00:00:00
    gra->GetXaxis()->SetNdivisions(505);
    gra->GetXaxis()->SetTimeOffset(offset);
    gra->GetXaxis()->SetTimeDisplay(1);
    return;
  }

  void SetXAxisDateTH2D(TH2D *th, int unixtime){
    int offset = unixtime - 788918400;// rootの基準は1995/1/1 00:00:00
    th->GetXaxis()->SetNdivisions(505);
    th->GetXaxis()->SetTimeOffset(offset);
    th->GetXaxis()->SetTimeDisplay(1);
    return;
  }

  void SetPallete(int color){
    gStyle->SetPalette(color);
    return;
  }

}

Bool_t MySignalHandler::Notify(){
  gSystem->ExitLoop();
  fprintf(stderr, " signal catched\n");
  return kTRUE;
}
