#include <kagali/KGLStdlib.h>
#include <kagali/KGLFrameIO.h>
#include <kagali/KGLNoisePSD.h>
#include <kagali/RealFFT.h>
#include <kagali/KGLLinearAlgebra.h>
#include <kagali/KGLTimeStamp.h>

#include "CorrelationFunction.h"

#include "parameters.h"

int main( int argc, char *argv[] ){

  if(argc!=5){
    fprintf(stderr, "usage : gene_data  duration[sec]  sigma  A_m  i_sim\n");
    exit(1);
  }

  int *r;
  double *y_tmp;

  double rho_pearson =0;
  KGLStatus *status = KGLCreateStatus();
  KGLRealFFTPlan *plan = NULL;  

  double fs    = FS;
  double duration = atof(argv[1]);
  double sigma = atof(argv[2]);

  int npoint = fs * duration;
  int nfreq=(npoint/2+1); 

  y_tmp  = (double*)malloc(sizeof(double)*npoint);
  r      = (int*)malloc(sizeof(int)*npoint);

  for(int i=0; i<npoint; i++){
    r[i]=i;
  }

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

  cdouble *noisef=NULL;
  KGLCallocAbortIfError(noisef, nfreq, cdouble,status);
  double *noiset=NULL;
  KGLCallocAbortIfError(noiset, npoint, double,status);
  double *white_noiset=NULL;
  KGLCallocAbortIfError(white_noiset, npoint, double,status);
  double *Sn=NULL;
  KGLCallocAbortIfError(Sn    , nfreq,double,status);


  /* read data here */
  double *sf1=NULL;
  KGLCallocAbortIfError(sf1 , nfreq,double,status);
  double *sf2=NULL;
  KGLCallocAbortIfError(sf2 , nfreq,double,status);
  double *sf3=NULL;
  KGLCallocAbortIfError(sf3 , nfreq,double,status);
  double *sf4=NULL;
  KGLCallocAbortIfError(sf4 , nfreq,double,status);
  double *sf5=NULL;
  KGLCallocAbortIfError(sf5 , nfreq,double,status);
  double *sf6=NULL;
  KGLCallocAbortIfError(sf6 , nfreq,double,status);
  double *sf7=NULL;
  KGLCallocAbortIfError(sf7 , nfreq,double,status);
  double *sf8=NULL;
  KGLCallocAbortIfError(sf8 , nfreq,double,status);
  double *sf9=NULL;
  KGLCallocAbortIfError(sf9 , nfreq,double,status);
  double *sf10=NULL;
  KGLCallocAbortIfError(sf10 , nfreq,double,status);
  noise_read(sf1, sf2, sf3, sf4, sf5, sf6,
	     sf7, sf8, sf9, sf10,
	     Sn, npoint, fs);

  /* for(int i=0; i<nfreq; i++){
   *   fprintf(stdout, "%e %e %e %e %e  %e %e %e %e %e  %e\n",
   *        i/(npoint/fs),
   *        sf1[i], sf2[i], sf3[i], sf4[i], sf5[i],
   *        sf6[i], sf7[i], sf8[i], sf9[i], sf10[i]
   *        );
   * } */


  double *noise_hsc=NULL;
  KGLCallocAbortIfError(noise_hsc, npoint, double,status);
  double *data_h_sc =NULL;
  KGLCallocAbortIfError(data_h_sc, npoint, double, status);

  double lambda = 1.064e-6; 	/* [m] */
  int i_sim_max= atoi(argv[4]);



  for(int i_sim=0; i_sim<i_sim_max; i_sim++){

    double g_factor = G_FACTOR; 		/* End Detection Bench */
    /* G 1e-20 ~ 9e-20 */
    
    generateAllDataFineWhite(rnd,
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
			     Sn,
			     white_noiset,
			     sf1, sf2, sf3, sf4, sf5,
			     sf6, sf7, sf8, sf9, sf10
			     );
    
    for(int i=0; i<npoint; i++){
      noise_hsc[i] = white_noiset[i];
    }
      
    rho_pearson=getrhoPearson(data_dx, noise_hsc,npoint);
    printf("%.15e ", rho_pearson);

    /* permutate test */
    shuffle(r,npoint);
    for(int j=0; j<npoint; j++) y_tmp[j] = noise_hsc[r[j]];
    for(int j=0; j<npoint; j++){
      noise_hsc[j] = y_tmp[j];
    }
    rho_pearson=getrhoPearson(data_dx,noise_hsc,npoint);

    printf("%.15e\n", rho_pearson);
 
  }

  free(data_dx);
  free(noiset_seis);
  free(noiset);
  free(noise_hsc);
  free(data_h_sc);
  free(white_noiset);
  
  KGLDestroyStatus(status);
  gsl_rng_free(rnd);
  return 0;
}
