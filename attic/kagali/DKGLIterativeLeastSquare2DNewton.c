
#include <kagali/KGLStdlib.h>
#include <kagali/KGLLeastSquareMethod.h>
#include <kagali/KGLLeastSquareFunc.h>
#include <kagali/KGLIterativeLeastSquare2DNewton.h>
#include <DKGLIterativeLeastSquare2DNewton.h>

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
  KGLDestroyStatus(status);
  
  return;
}
