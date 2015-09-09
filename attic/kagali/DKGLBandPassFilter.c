
#include <kagali/KGLStdlib.h>
#include <kagali/KGLBandPassFilter.h>
#include <DKGLBandPassFilter.h>
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
  KGLFIRBandPassFilterKernel(status,bpfh,iWindow,nKernel,flow/fs,fhigh/fs);
  
  for(int i = 0; i < npoint; i += iratio){
    dataout[i] = 0;
    if(i >= (nKernel-1)/2){
      for(int j = 0; j < nKernel; j++){
	dataout[i] += datain[i + (nKernel-1)/2 - j] * bpfh[j];
      }
    }
  }
  
  KGLDestroyStatus(status);
  
  return;
}


void DKGLButterworthBandPassFilter( //begin{proto}
    double *dataout, /**< [out] data */
    double *datain,  /**< [in] data */
    int npoint,      /**< [in] data length */
    double fs,       /**< [in] sampling frequency [Hz] */
    double flow,     /**< [in] lower cutoff frequency [Hz] */
    double fhigh     /**< [in] higher cutoff frequency [Hz] */
    )  //end{proto}
{
  KGLStatus *status = KGLCreateStatus();
  
  int order = 6;
  double *num_coeff = NULL;
  double *den_coeff = NULL;
  double *datatmp = NULL;
  KGLCalloc(num_coeff,order+1,double,status);
  KGLCalloc(den_coeff,order+1,double,status);
  KGLCalloc(datatmp,npoint,double,status);
  
  if(flow < fs/2.0 && (fhigh > 0 && fhigh < fs/2.0)){
    KGLButterworthLowPassFilterKernel(status,num_coeff,den_coeff,npoint,fhigh,fs,order);
    KGLZeroPhaseFilter(status,datatmp,datain,npoint,num_coeff,den_coeff,order);
    KGLButterworthHighPassFilterKernel(status,num_coeff,den_coeff,npoint,flow,fs,order);
    KGLZeroPhaseFilter(status,dataout,datatmp,npoint,num_coeff,den_coeff,order);
    
  }else if(fhigh <= 0 || flow >= fs/2.0){
    for(int i = 0; i < npoint; i++) dataout[i] = 0;
    
  }else if(flow == 0 && (fhigh > 0 && fhigh < fs/2.0)){
    KGLButterworthLowPassFilterKernel(status,num_coeff,den_coeff,npoint,fhigh,fs,order);
    KGLZeroPhaseFilter(status,dataout,datain,npoint,num_coeff,den_coeff,order);
    
  }else if(flow == 0 && fhigh == fs/2.0){
    for(int i = 0; i < npoint; i++) dataout[i] = datain[i];
    
  }else if(fhigh >= fs/2.0){
    KGLButterworthHighPassFilterKernel(status,num_coeff,den_coeff,npoint,flow,fs,order);
    KGLZeroPhaseFilter(status,dataout,datain,npoint,num_coeff,den_coeff,order);
  }
  
  KGLDestroyStatus(status);
  
  return;
}
