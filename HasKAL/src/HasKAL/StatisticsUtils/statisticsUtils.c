/************************************************/
/* File   : libStatisticsUtils.c                */
/* Author : Hirotaka Yuzurihara                 */
/* Time-stamp: "2014-07-28 16:56:18 yuzurihara" */
/************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <time.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_cdf.h>


double calculatePeasonCorrelation(double *data1, double *data2, int dataLength){

  double mean1 = 0.0;
  double mean2 = 0.0;
  double var1  = 0.0;
  double var2  = 0.0;
  double cov   = 0.0;

  int i;

  for(i=0; i < dataLength ; i++) {
    mean1 += data1[i] / dataLength;
    mean2 += data2[i] / dataLength;
  }
  for(i=0; i<dataLength; i++) {
    var1 += (data1[i]*data1[i] - mean1*mean1) / (dataLength - 1.0);
    var2 += (data1[i]*data2[i] - mean2*mean2) / (dataLength - 1.0);
    cov  += (data1[i]*data2[i] - mean1*mean2) / (dataLength - 1.0);
  }

  return cov/sqrt(var1*var2);
}


double permutationTestPeasonCorrelation(double *data1, double *data2, int dataLength, int repeatTimesOfTest){


  /* GSL setup */
  gsl_rng_type *T = (gsl_rng_type *)gsl_rng_default;
  gsl_rng *rnd = gsl_rng_alloc(T);
  /* random seed is set by system clock */
  gsl_rng_set (rnd, time(NULL));

  int i, j, k;
  int tmp;
  int countTail = 0;
  double rhoPeason_0 = 0;
  double rhoPeason   = 0;

  double *data2mod;
  int *index;


  /* memory allocate */
  data2mod = (double*)malloc(sizeof(double*)*dataLength);
  index = (int*)malloc(sizeof(int*)*dataLength);

  /* First, calculate peason correlation coefficient */
  rhoPeason_0 = calculatePeasonCorrelation(data1, data2, dataLength);

  /* initialize index list before randomize*/
  for(i=0; i < dataLength; i++) index[i] = i;


  for(j=0; j < repeatTimesOfTest; j++){

     /* permutation by Fisher-Yates shuffle */
    for(i=dataLength-1; i>=1; i--) {
      /* generates a random number on [0,1)-real-interval */
      k = (int)floor(gsl_rng_uniform(rnd)*(i+1));
      tmp=index[i];
      index[i]=index[k];
      index[k]=tmp;
    }

    /* re-substitution data2 using randomized index*/
    for(i=0; i<dataLength; i++) data2mod[i]=data2[index[i]];
    
    /* re-calculate peason correlation coefficient */
    rhoPeason = calculatePeasonCorrelation(data1, data2mod, dataLength);

    if(fabs(rhoPeason) > fabs(rhoPeason_0) ) countTail++;
  }

  fprintf(stderr, " *** check : tail counter = %d\n", countTail);
  
  free(data2mod);
  gsl_rng_free(rnd);

  return countTail/(double)repeatTimesOfTest;
}
