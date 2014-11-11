/******************************************
 *     File Name: cFunction.cc
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/11/11 15:01:22
 ******************************************/

#include "cFunction.h"

using namespace std;

extern "C" {
  int cModified(TCanvas *canvas){
    canvas->Modified();
    return 0;
  }

  int cUpdate(TCanvas *canvas){
    canvas->Update();
    return 0;
  }
}


