 
#include <kagali/KGLStdlib.h>
#include <kagali/KGLBandPassFilter.h>
#include <kagali/KGLLeastSquareMethod.h>
#include <kagali/KGLLeastSquareFunc.h>
#include <kagali/KGLIterativeLeastSquare2DNewton.h>
#include <kagali/KGLInterpolation.h>
#include "DKGLUtils.h"
#include "chirplet_func.c"
#include <gsl/gsl_sf_bessel.h>


void DKGLFIRBandPassFilter( //begin{proto}
    double *dataout, /**< [out] data */
    double *datain,  /**< [in] data */
    int npoint,      /**< [in] data length */
    double fs,       /**< [in] sampling frequency [Hz] */
    double flow,     /**< [in] lower cutoff frequency [Hz] */
    double fhigh     /**< [in] higher cutoff frequency [Hz] */
    )  //end{proto}
{
  KGLStatus *status = KGLCreateStatus();
  KGLWinFunc iWindow = kgl_wf_hann;
  int nKernel = 191;
  int iratio = (int)(fs/fhigh/2.0);
  //  int npoint2 = npoint/iratio;
  
  double *bpfh = NULL;
  KGLCallocAbortIfError(bpfh,nKernel,double,status);
  double Astop = 56.0;
  KGLFIRBandPassFilterKernel(status,bpfh,iWindow,nKernel,Astop,flow/fs,fhigh/fs);
  
  for(int i = 0; i < npoint; i += iratio){
    dataout[i] = 0;
    if(i >= (nKernel-1)/2){
      for(int j = 0; j < nKernel; j++){
	dataout[i] += datain[i + (nKernel-1)/2 - j] * bpfh[j];
      }
    }
  }
  
  free(bpfh);
  
  KGLDestroyStatus(status);
  
  return;
}


void DKGLButterworthBandPassFilter( //begin{proto}
    double *dataout, /**< [out] data */
    double *datain,  /**< [in] data */
    int npoint,      /**< [in] data length */
    double fs,       /**< [in] sampling frequency [Hz] */
    double flow,     /**< [in] lower cutoff frequency [Hz] */
    double fhigh,    /**< [in] higher cutoff frequency [Hz] */
    int order        /**< [in] filter order */
    )  //end{proto}
{
  KGLStatus *status = KGLCreateStatus();
  
  double *num_coeff = NULL;
  double *den_coeff = NULL;
  double *datatmp = NULL;
  KGLCalloc(num_coeff,order+1,double,status);
  KGLCalloc(den_coeff,order+1,double,status);
  KGLCalloc(datatmp,npoint,double,status);
  
  if((flow > 0 && flow < fs/2.0) && (fhigh > 0 && fhigh < fs/2.0)){
    KGLButterworthLowPassFilterKernel(status,num_coeff,den_coeff,npoint,fhigh,fs,order);
    KGLZeroPhaseFilter(status,datatmp,datain,npoint,num_coeff,den_coeff,order,1);
    KGLButterworthHighPassFilterKernel(status,num_coeff,den_coeff,npoint,flow,fs,order);
    KGLZeroPhaseFilter(status,dataout,datatmp,npoint,num_coeff,den_coeff,order,1);
    
  }else if(flow >= fs/2.0){
    for(int i = 0; i < npoint; i++) dataout[i] = 0;
    
  }else if(flow == 0 && (fhigh > 0 && fhigh < fs/2.0)){
    KGLButterworthLowPassFilterKernel(status,num_coeff,den_coeff,npoint,fhigh,fs,order);
    KGLZeroPhaseFilter(status,dataout,datain,npoint,num_coeff,den_coeff,order,1);
    
  }else if(flow == 0 && fhigh >= fs/2.0){
    for(int i = 0; i < npoint; i++) dataout[i] = datain[i];
    
  }else if(fhigh >= fs/2.0){
    KGLButterworthHighPassFilterKernel(status,num_coeff,den_coeff,npoint,flow,fs,order);
    KGLZeroPhaseFilter(status,dataout,datain,npoint,num_coeff,den_coeff,order,1);
  }
  
  KGLDestroyStatus(status);
  
  return;
}


void DKGLButterworthBandPassSOSFilter( //begin{proto}
    double *dataout, /**< [out] data */
    double *datain,  /**< [in] data */
    int npoint,      /**< [in] data length */
    double fs,       /**< [in] sampling frequency [Hz] */
    double flow,     /**< [in] lower cutoff frequency [Hz] */
    double fhigh,    /**< [in] higher cutoff frequency [Hz] */
    int order        /**< [in] filter order */
    )  //end{proto}
{
  KGLStatus *status = KGLCreateStatus();
  
  int nrows = (order + (order%2))/2; 
  double gain = 1.0;
  double **sos = NULL;
  double *datatmp = NULL;
  KGLCallocMatrix(sos,nrows,6,double,status);
  KGLCalloc(datatmp,npoint,double,status);
  
  if((flow > 0 && flow < fs/2.0) && (fhigh > 0 && fhigh < fs/2.0)){
    KGLButterworthLowPassSOSFilterKernel(status,&gain,sos,npoint,fhigh,fs,order);
    KGLZeroPhaseSOSFilter(status,datatmp,datain,npoint,gain,sos,order,1);
    KGLButterworthHighPassSOSFilterKernel(status,&gain,sos,npoint,flow,fs,order);
    KGLZeroPhaseSOSFilter(status,dataout,datatmp,npoint,gain,sos,order,1);
    
  }else if(flow >= fs/2.0){
    for(int i = 0; i < npoint; i++) dataout[i] = 0;
    
  }else if(flow == 0 && (fhigh > 0 && fhigh < fs/2.0)){
    KGLButterworthLowPassSOSFilterKernel(status,&gain,sos,npoint,fhigh,fs,order);
    KGLZeroPhaseSOSFilter(status,dataout,datain,npoint,gain,sos,order,1);
    
  }else if(flow == 0 && fhigh >= fs/2.0){
    for(int i = 0; i < npoint; i++) dataout[i] = datain[i];
    
  }else if(fhigh >= fs/2.0){
    KGLButterworthHighPassSOSFilterKernel(status,&gain,sos,npoint,flow,fs,order);
    KGLZeroPhaseSOSFilter(status,dataout,datatmp,npoint,gain,sos,order,1);
  }
  
  KGLDestroyStatus(status);
  
  return;
}


void DKGLIterativeLeastSquare2DNewton( //begin{proto}
    double *Afit,  /**< [out] fitted amplitudes       (array size of nsig) */
    double *ffit,  /**< [out] fitted frequencies [Hz] (array size of nsig) */
    double *pfit,  /**< [out] fitted phases [rad]     (array size of nsig) */
    double *frame, /**< [in] time-series data         (array size of nframe) */
    double fs,     /**< [in] sampling frequency [Hz] */
    int nframe,    /**< [in] data (frame) length */
    int nsig       /**< [in] number of signals to extract*/
    ) //end{proto}
{
  KGLStatus *status = KGLCreateStatus();
  
  int nitr = 40;
  double Fcostthr=pow(10,-8);
  double a2thr = 0;
  double mu0 = 2.0;
  double nu0 = 2.0;
  
  KGLIterativeLeastSquare2DNewton(status,Afit,ffit,pfit,frame,fs,nframe,
				  Fcostthr,a2thr,nsig,nitr,mu0,nu0);
  
  for(int i = 0; i < nsig; i++){    
    if(Afit[i] != Afit[i] 
       || fabs(ffit[i]) < 1e-6
       || fabs(pfit[i]) < 1e-5
       ){
      Afit[i] = 0;
      ffit[i] = 0;
      pfit[i] = 0;
    }
  }
  
  KGLDestroyStatus(status);
  
  return;
}


void DKGLChirpletMain( //begin{proto}
    double *freqa,     /**< [out] Frequency values of length nframe */
    double *cost,      /**< [out] */
    double *frame,     /**< [in] time-series data */
    double fs,         /**< [in] sampling frequency [Hz] */
    double alpha,      /**< [in] Calculates BP for lengths 1,...,maxLength */
    int ipath,         /**< [in] Number of paths used */
    int nframe         /**< [in] data (frame) length */
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

    free(timegrid);
    free(freqgrid);
    free(timea);
  }
  free(paths);
  free(costpath);
  
  KGLDestroyStatus(status);
  
  return;
}

