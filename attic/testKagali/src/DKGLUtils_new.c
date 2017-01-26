
#include <kagali/KGLStdlib.h>
#include <kagali/KGLBandPassFilter.h>
#include <kagali/KGLLeastSquareMethod.h>
#include <kagali/KGLLeastSquareFunc.h>
#include <kagali/KGLIterativeLeastSquare2DNewton.h>
#include "DKGLUtils_new.h"
//#include "chirplet_func.c"
#include <gsl/gsl_sf_bessel.h>

void DKGLChirpletMain( //begin{proto}
    double *timea,          /**< [out] */
    double *freqa,          /**< [out] */
    double *frame,          /**< [in] time-series data */
    double fs,              /**< [in] sampling frequency [Hz] */
    double alpha,           /**< [in] Calculates BP for lengths 1,...,maxLength */
    int ipath,              /**< [in] Number of paths used */
    int nframe              /**< [in] data (frame) length */
    ) //end{proto}
{
  KGLStatus *status = KGLCreateStatus();
  int maxLength = (int)alpha;
  char *COMPLEX_OR_REAL = "REAL"; 
  char *XTTYPE = "PLAIN";
  char *STATTYPE = "BPFORPLOTTING";
  char *INJECTION = "FromText";
  int **paths = NULL;
  double *cost = NULL;
  if(strcmp(STATTYPE,"SP") == 0 || strcmp(STATTYPE,"MCTR") == 0){
    KGLCallocMatrixAbortIfError(paths,1,nframe*nframe,int,status);
    KGLCallocAbortIfError(cost,nframe*nframe,double,status);
  }
  if(strcmp(STATTYPE,"BPFORPLOTTING") == 0){
    KGLCallocMatrixAbortIfError(paths,maxLength,maxLength+1,int,status);
    KGLCallocAbortIfError(cost,maxLength,double,status);
  }
  
  double complex *cframe = NULL;
  KGLCalloc(cframe,nframe,double complex,status);
  for(int i = 0; i < nframe; i++){
    cframe[i] = frame[i];
  }
  KGLChirpletMain(status,paths,cost,cframe,
		  COMPLEX_OR_REAL,XTTYPE,STATTYPE,
		  INJECTION,(int)fs,alpha,nframe);
  KGLAbortIfError(status);  
  free(cframe);
  
  int jframe = ceil(log2(nframe)); // dyadic length of signal
  int minfreq = 0; // restricting the chirplet graph to nonnegative frequencies
  int maxfreq = nframe-1;
  int nfreqs = maxfreq-minfreq+1;
  //int csc = 0;
  int fsc = jframe-1;
  int jend = 0;
  if(strcmp(STATTYPE,"SP") == 0){
    jend = nframe;
  }
  if(strcmp(STATTYPE,"BPFORPLOTTING") == 0){
    jend = ipath+1;
  }
  if(strcmp(STATTYPE,"SP") == 0 ||
     strcmp(STATTYPE,"BPFORPLOTTING") == 0){
    
    for (int j = 0; j <= jend; j++){
      if(paths[ipath][j] == 0) break;
      int p = paths[ipath][j];
      int freq = (p - 1) % nfreqs;
      int time = (p - freq - 1)/(nfreqs/(int)pow(2,jframe-fsc));
      freq *= fs/nframe;
      timea[j] = time/fs;
      freqa[j] = freq;
    }
  }
  
  KGLDestroyStatus(status);
  
  return;
}
