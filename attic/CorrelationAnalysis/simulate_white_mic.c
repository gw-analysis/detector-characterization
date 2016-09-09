#include <kagali/KGLStdlib.h>
#include <kagali/KGLFrameIO.h>
#include <kagali/KGLNoisePSD.h>
#include <kagali/RealFFT.h>
#include <kagali/KGLLinearAlgebra.h>
#include <kagali/KGLDetectorDesignSensitivity.h>
#include <kagali/KGLTimeStamp.h>
#include "./example_MIC/libmine/mine.h"
#include "CorrelationFunction.h"

#include "parameters.h"

int main( int argc, char *argv[] ){

  if(argc!=5){
    fprintf(stderr, "usage : gene_data  duration[sec]  sigma  A_m  i_sim\n");
    exit(1);
  }

  mine_problem prob;
  mine_parameter param;
  mine_score *score;
  char *ret;
  int *r;
  double *y_tmp;

  double rho_mic =0;
  KGLStatus *status = KGLCreateStatus();
  KGLRealFFTPlan *plan = NULL;  

  double fs    = FS;
  double duration = atof(argv[1]);
  double sigma = atof(argv[2]);

  int npoint = fs * duration;
  int nfreq=(npoint/2+1); 


  prob.x = (double*)malloc(sizeof(double)*npoint);
  prob.y = (double*)malloc(sizeof(double)*npoint);
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
   * 	    i/(npoint/fs),
   * 	    sf1[i], sf2[i], sf3[i], sf4[i], sf5[i],
   * 	    sf6[i], sf7[i], sf8[i], sf9[i], sf10[i]
   * 	    );
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
    
    // i/fs, data_dx[i], data_h_sc[i], data_h_sc[i]+noiset[i], noiset[i]);
    // analysi data : data_dx[i] and data_h_sc[i]+noiset[i]
    for(int i=0; i<npoint; i++){
      prob.x[i] = data_dx[i];
      //prob.y[i] = data_h_sc[i] + noiset[i];
      prob.y[i] = white_noiset[i];
    }
    
    /* set the parameters */
    param.alpha = 0.6;
    param.c = 15;
    prob.n = npoint;
    
    ret = mine_check_parameter(&param);
    if (ret){
      fprintf(stderr, "ERROR: %s\n", ret);
      return 1;
    }
    /* compute score */
    score = mine_compute_score(&prob, &param);
    rho_mic = mine_mic(score);
    printf("%.15e ", rho_mic);
  
  
    /* permutate test */
    shuffle(r,npoint);
    for(int j=0; j<npoint; j++) y_tmp[j]=prob.y[r[j]];
    for(int j=0; j<npoint; j++){
      prob.y[j]=y_tmp[j];
    }
    /* if(fabs(tmp_rho)>fabs(rhoo)){
     *   tail+=1.0; */
  
    /* compute score */
    score = mine_compute_score(&prob, &param);
    if (score == NULL){
      fprintf(stderr, "ERROR: mine_compute_score()\n");
      return 1;
    }
    printf("%.15e\n", mine_mic(score));
  
  
   
  }
  free(data_dx);
  free(noiset_seis);
  free(noisef_seis);
  free(noisef);
  free(Sn);
  free(sf1); free(sf2); free(sf3); free(sf4); free(sf5); 
  free(sf6); free(sf7); free(sf8); free(sf9); free(sf10); 
  free(noiset);
  free(noise_hsc);
  free(data_h_sc);
  free(white_noiset);

  mine_free_score(&score);    
  KGLDestroyStatus(status);
  gsl_rng_free(rnd);
  return 0;
}
