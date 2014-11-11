/******************************************
 *     File Name: cFunction.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/11/11 17:33:01
 ******************************************/

#ifndef I_GUARD_CFUNCTION_H
#define I_GUARD_CFUNCTION_H

#include <TCanvas.h>

extern "C" {
  int cModified(TCanvas *canvas);
  int cUpdate(TCanvas *canvas);
}

#endif /*  I_GUARD_CFUNCTION_H  */

