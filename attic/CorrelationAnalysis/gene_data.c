#include <kagali/KGLStdlib.h>
#include <kagali/KGLNoisePSD.h>
#include <kagali/RealFFT.h>
#include <kagali/KGLLinearAlgebra.h>
#include <kagali/KGLTimeStamp.h>
#include "CorrelationFunction.h"
#include "parameters.h"

int main( int argc, char *argv[] ){

  if(argc!=4){
    fprintf(stderr, "usage : gene_data duration[sec] sigma A_m\n");
    exit(1);
  }


  KGLStatus *status = KGLCreateStatus();
  KGLRealFFTPlan *plan = NULL;  
  char fname[100]="";
  FILE *fp=NULL;

  double fs    = FS;
  //double fs    = 512.0;
  //double duration = DURATION;
  double duration = atof(argv[1]);
  double sigma = atof(argv[2]);

  int npoint = fs * duration;
  int nfreq=(npoint/2+1); 

  fprintf(stderr, "fs = %f\n", fs);
  fprintf(stderr, "duration = %f\n", duration);
  fprintf(stderr, "npoint = %d\n", npoint);
  fprintf(stderr, "sigma = %f\n", sigma);

  /* setup random generator in GSL */
  gsl_rng_type *TT = (gsl_rng_type *)gsl_rng_default;
  gsl_rng      *rnd = gsl_rng_alloc(TT);
  // If you want initialize seed using time(now), change comment out below
  gsl_rng_set (rnd, time(NULL));

  double *data_dx =NULL;
  KGLCallocAbortIfError(data_dx, npoint, double, status);
  double *noiset_seis =NULL;
  KGLCallocAbortIfError(noiset_seis, npoint, double, status);
  cdouble *noisef_seis =NULL;
  KGLCallocAbortIfError(noisef_seis, nfreq, cdouble,status);

  double f_m = F_M;
  double a_m = A_M;
  a_m = atof(argv[3]);
  fprintf(stderr, "f_m = %e\n", f_m);
  fprintf(stderr, "a_m = %e\n", a_m);
  fprintf(stderr, "tau = %e\n", TAU);


  KGLNPSD Detector;
  Detector = XKGLNoisePowerSpecDenS2E("bKAGRA-vrseb");

  cdouble *noisef=NULL;
  double *Sn=NULL;
  KGLCallocAbortIfError(noisef, nfreq, cdouble,status);
  KGLCallocAbortIfError(Sn    , nfreq,double,status);
  double *noiset=NULL;
  KGLCallocAbortIfError(noiset, npoint, double,status);
  KGLReadNoisePSD(status,Sn,Detector,npoint,fs);

  double *noise_hsc=NULL;
  KGLCallocAbortIfError(noise_hsc, npoint, double,status);
  double *data_h_sc =NULL;
  KGLCallocAbortIfError(data_h_sc, npoint, double, status);


  double lambda = 1.064e-6; 	/* [m] */

  double g_factor = G_FACTOR; 		/* End Detection Bench */
  /* G 1e-20 ~ 9e-20 */
  fprintf(stderr, "g factor = %e\n", g_factor);
  fprintf(stderr, "lambda = %e\n", lambda);


  generateAllData(rnd,
		  plan,
		  f_m, 
		  a_m,
		  lambda,
		  g_factor,
		  duration,
		  fs,
		  noiset_seis,
		  data_dx,
		  data_h_sc,
		  noiset,
		  noisef_seis,
		  noisef,
		  Sn);

  /* output check of simulation noise in time domain */  
  sprintf(fname, "./dat/data_dx_hsc_fs%.0f_f_m%.0f_a%.1e_duration_%.1f_tau%.1e.txt", fs, f_m, a_m, duration, TAU);
  fp = fopen(fname,"w");
  for(int i=0; i<npoint; i++){
    fprintf(fp, "%e  %e  %e  %e  %e  %e\n",
	    i/fs, data_dx[i], data_h_sc[i], data_h_sc[i]+noiset[i], noiset[i], noiset_seis[i]);
    
    //if(i%30==0 && i!=0) fprintf(fp, "\n\n");
  }
  //  時間  dx  h_sc  h_sc+noise  noise
  // analysis data -> awk '{print $2, $4}'

  
  free(data_dx);
  free(noiset_seis);
  free(noisef_seis);
  free(noisef);
  free(Sn);
  free(noiset);
  free(noise_hsc);
  free(data_h_sc);
  KGLDestroyStatus(status);

  return 0;
}
