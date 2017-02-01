
#include <kagali/KGLStdlib.h>
#include <kagali/KGLInterpolation.h>
#include "DKGLUtils_new.h"

void DKGLChirpletMain( //begin{proto}
    double *freqa,          /**< [out] Frequency values of length nframe */
    double *cost,           /**< [out] */
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
  double *costpath = NULL;
  if(strcmp(STATTYPE,"SP") == 0 || strcmp(STATTYPE,"MCTR") == 0){
    KGLCallocMatrixAbortIfError(paths,1,nframe*nframe,int,status);
    KGLCallocAbortIfError(costpath,nframe*nframe,double,status);
  }
  if(strcmp(STATTYPE,"BPFORPLOTTING") == 0){
    KGLAssert(status,(int)alpha > ipath,"alpha must be greater than ipath.\n");
    KGLEndAssert(status);
    KGLCallocMatrixAbortIfError(paths,maxLength,maxLength+1,int,status);
    KGLCallocAbortIfError(costpath,maxLength,double,status);
  }
  
  double complex *cframe = NULL;
  KGLCalloc(cframe,nframe,double complex,status);
  for(int i = 0; i < nframe; i++){
    cframe[i] = frame[i];
  }
  KGLChirpletMain(status,paths,costpath,cframe,
		  COMPLEX_OR_REAL,XTTYPE,STATTYPE,
		  INJECTION,(int)fs,alpha,nframe);
  KGLAbortIfError(status);  
  free(cframe);
  
  int jframe = ceil(log2(nframe)); // dyadic length of signal
  int minfreq = 0; // restricting the chirplet graph to nonnegative frequencies
  int maxfreq = nframe-1;
  int nfreqs = maxfreq-minfreq+1;
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
      
    double *timegrid = NULL;
    double *freqgrid = NULL;
    double *timea = NULL;
    KGLCallocAbortIfError(timegrid,jend+1,double,status);
    KGLCallocAbortIfError(freqgrid,jend+1,double,status);
    KGLCallocAbortIfError(timea,nframe,double,status);
    
    for (int j = 0; j <= jend; j++){
      if(paths[ipath][j] == 0) break;
      int p = paths[ipath][j];
      int freq = (p - 1) % nfreqs;
      int time = (p - freq - 1)/(nfreqs/(int)pow(2,jframe-fsc));
      freq *= fs/nframe;
      timegrid[j] = time/fs;
      freqgrid[j] = freq;
    } 
    for (int j = 0; j < nframe; j++) timea[j] = j/fs;
    KGLLinearInterpolation(status,freqa,timea,freqgrid,timegrid,nframe,jend+1);
    KGLAbortIfError(status);

    *cost = costpath[ipath];
  }
  
  KGLDestroyStatus(status);
  
  return;
}
