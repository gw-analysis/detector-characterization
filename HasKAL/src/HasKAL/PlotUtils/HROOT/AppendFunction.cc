

#include "AppendFunction.h"

using namespace std;

extern "C" {
  int SetRangeUser(TAxis *axis, double min, double max){
    axis->SetRangeUser(min, max);
    return 0; // ダミーの返り値
  }

  int hSetGrid (TCanvas *canvas){
    canvas->SetGrid();
    return 0;
  }

  int cModified(TCanvas *canvas){
    canvas->Modified();
    return 0;
  }

  int cUpdate(TCanvas *canvas){
    canvas->Update();
    return 0;
  }

  int AddSignalHandle (){
    gSystem->AddSignalHandler(new MySignalHandler(kSigInterrupt) );
    return 0; // Haskell上で必要なダミーの返り値(C++で使う場合はvoidで良い)
  }

  int SetRangeTH(TH1 *hist, double xmin, double xmax, double ymin, double ymax){
    if (xmin < xmax) hist->GetXaxis()->SetRangeUser(xmin, xmax);
    if (ymin < ymax) hist->GetYaxis()->SetRangeUser(ymin, ymax);
    return 0;
  }
}

Bool_t MySignalHandler::Notify(){
  gSystem->ExitLoop();
  fprintf(stderr, " signal catched\n");
  return kTRUE;
}
