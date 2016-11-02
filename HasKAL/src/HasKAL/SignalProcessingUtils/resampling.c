
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "resampling.h"

void downsample(unsigned n, unsigned xs, double* x, unsigned rs, double* r){
  int i;
  for (i = 0; i < rs; i++)
    r[i] = x[i*n];
}


