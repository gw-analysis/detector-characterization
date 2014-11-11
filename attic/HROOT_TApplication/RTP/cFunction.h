/******************************************
 *     File Name: cFunction.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/11/11 14:58:10
 ******************************************/

#ifndef I_GUARD_CFUNCTION_H
#define I_GUARD_CFUNCTION_H

//#include <stdio.h>
#include <TSystem.h>
#include <TSysEvtHandler.h>
#include <TCanvas.h>
#include <TH1.h>

extern "C" {
  int cModified(TCanvas *canvas);
  int cUpdate(TCanvas *canvas);
}

#endif /*  I_GUARD_CFUNCTION_H  */

