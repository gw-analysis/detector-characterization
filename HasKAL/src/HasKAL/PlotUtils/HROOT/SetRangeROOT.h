/******************************************
 *     File Name: SetRangeROOT.h
 *        Author: Takahiro Yamamoto
 * Last Modified: 2014/10/02 17:43:52
 ******************************************/

#ifndef I_GUARD_SETRANGEROOT_H
#define I_GUARD_SETRANGEROOT_H

#include <TH1.h>

extern "C" {
  int SetRangeUser(TAxis *axis, double min, double max);
}

#endif /*  I_GUARD_SETRANGEROOT_H  */

