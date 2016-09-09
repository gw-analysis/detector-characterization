#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <complex.h>

#include <time.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_cdf.h>

#include <gsl/gsl_sort.h>
#include <gsl/gsl_sort_vector.h>
#include "CorrelationFunction.h"
#include "parameters.h"

#include <kagali/KGLStdlib.h>
#include <kagali/KGLNoisePSD.h>
#include <kagali/RealFFT.h>
#include <kagali/KGLLinearAlgebra.h>
#include <kagali/KGLTimeStamp.h>

#include <kagali/KGLInterpolation.h>

#define FRMIN 5.0

double getrhoPearson(double val1[],double val2[],int len) {
  int i;
  double mean1,mean2;
  double var1,var2,cov;

  mean1=0.0;
  mean2=0.0;
  var1=0.0;
  var2=0.0;
  cov=0.0;

  for(i=0; i<len; i++) {
    mean1+=val1[i]/len;
    mean2+=val2[i]/len;
  }
  for(i=0; i<len; i++) {
    var1+=(val1[i]*val1[i]-mean1*mean1)/(len-1.0);
    var2+=(val2[i]*val2[i]-mean2*mean2)/(len-1.0);
    cov+=(val1[i]*val2[i]-mean1*mean2)/(len-1.0);
  }

  return cov/sqrt(var1*var2);

}

void shuffle(int ran[],int len) {
  /* GSLのセットアップをする */
  /* どのような乱数が必要か？ */
  /* GSL set-up */
  gsl_rng_type *T = (gsl_rng_type *)gsl_rng_default;
  /* 乱数発生器 */
  gsl_rng *rnd = gsl_rng_alloc(T);
  /* システムクロックを使って乱数の初期値を設定 */
  gsl_rng_set (rnd, time(NULL));

  int i,j;
  int tmp;
  double genrand_real2(void);
  for(i=len-1; i>=1; i--) {
    //j=(int)(genrand_real2()*(i+1));
    j=(int)(gsl_rng_uniform(rnd)*(i+1));
    tmp=ran[i];
    ran[i]=ran[j];
    ran[j]=tmp;
  } // permutation by Fisher-Yates shuffle
  gsl_rng_free(rnd);
   return;
}

double getrhoDistantCor(double val1[],double val2[],int len) {

  int i,j;
  double var1,var2,cov;
  double am[MAX_PTS],bm[MAX_PTS];
  double amm,bmm;

  for(i=0;i<len;i++) {
    am[i]=bm[i]=0.0;
    for(j=0;j<len;j++) {
      am[i]+=fabs(val1[i]-val1[j])/len;
      bm[i]+=fabs(val2[i]-val2[j])/len; 
    }
  }
  amm=bmm=0.0;
  for(i=0;i<len;i++) {
    amm+=am[i]/len;
    bmm+=bm[i]/len;
  }

  cov=0.0;
  for(i=0;i<len;i++) {
    for(j=0;j<len;j++) {
      cov+=(fabs(val1[i]-val1[j])-am[i]-am[j]+amm)
          *(fabs(val2[i]-val2[j])-bm[i]-bm[j]+bmm)/len/len;
    }
  }

  var1=0.0;
  var2=0.0;
  for(i=0;i<len;i++) {
    for(j=0;j<len;j++) {
    var1+=pow(fabs(val1[i]-val1[j])-am[i]-am[j]+amm,2.0)/len/len;
    var2+=pow(fabs(val2[i]-val2[j])-bm[i]-bm[j]+bmm,2.0)/len/len;
    }
  }

  return sqrt(cov/sqrt(var1*var2));

}




int generateAllData(gsl_rng *rnd, KGLRealFFTPlan *plan,
		    double f_m, double a_m, double lambda,
		    double g_factor, double duration, double fs,
		    double *noiset_seis, double *data_dx, double *data_h_sc,
		    double *noiset, 
		    cdouble *noisef_seis, cdouble *noisef, double *Sn) {
  
  KGLStatus *status = KGLCreateStatus();

  int npoint = fs * duration;
  int nfreq=(npoint/2+1); 
  double df=fs/npoint;
  
  /* generate seismic noise */
  for(int i=0; i<nfreq; i++){
    double f = i/duration;
    if(f>5.0){
      double sig = A_SEISMIC/sqrt(fs/npoint)/2.0 / f / f;
      noisef_seis[i] = gsl_ran_gaussian(rnd,sig) + I*gsl_ran_gaussian(rnd, sig);
    }else{
      /* f = 5.0;
       * double sig = A_SEISMIC/sqrt(fs/npoint)/2.0 / f / f;
       * noisef_seis[i] = gsl_ran_gaussian(rnd,sig) + I*gsl_ran_gaussian(rnd, sig); */
      noisef_seis[i] = 0 + I*0;
    }
  }
  KGLReverseRealFFT( status, &plan, noiset_seis, noisef_seis, npoint);
  KGLAbortIfError(status);  

  for(int i=0; i<npoint; i++){
    double t = i/fs;
    data_dx[i]  = a_m * sin(2.0*KGL_PI*f_m * t) * exp(-t/TAU) + noiset_seis[i]*df;
  }

  double phase_x_0 = 2 * KGL_PI * gsl_rng_uniform (rnd);
  for(int i=0; i<npoint; i++){
    //data_h_sc[i] = g_factor * sin (4.0*KGL_PI/lambda * (x_0 + data_dx[i]));
    data_h_sc[i] = g_factor * sin (4.0*KGL_PI/lambda * (data_dx[i]) + phase_x_0);
  }

  // below function should be replaced ....
  KGLGenerateGaussNoiseGSL(status, noisef, npoint, Sn, 5, fs, rnd);
  KGLAbortIfError(status);
  
  /* Get random noise data in timedomain, using IFFT*/
  KGLReverseRealFFT( status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);

  for(int i=0; i<npoint; i++){
    noiset[i] = noiset[i] * df;
  }

  /* whiten data */
  //whiten_data(status, noiset, npoint, fs, noisef, Sn, plan);

  KGLDestroyStatus(status);
  return 0;
}




int generateAllDataWhite(gsl_rng *rnd, KGLRealFFTPlan *plan,
			 double f_m, double a_m, double lambda,
			 double g_factor, double duration, double fs,
			 double *noiset_seis, double *data_dx, double *data_h_sc,
			 double *noiset, 
			 cdouble *noisef_seis, cdouble *noisef, double *Sn,
			 double *white_noiset
			 ) {

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
  KGLStatus *status = KGLCreateStatus();
  int npoint = fs * duration;  

  for(int i=0; i<npoint; i++){
    white_noiset[i] = noiset[i] + data_h_sc[i];
    noiset[i] = noiset[i] + data_h_sc[i];
  }
  whiten_data(status, white_noiset, npoint, fs, noisef, Sn, plan);  
  return 0;
}



void whiten_data(KGLStatus *status,
		 double *noiset,
		 int npoint,
		 double fs,
		 cdouble *noisef,
		 double *Sn,
		 KGLRealFFTPlan *plan){  

  /* FFT */
  KGLForwardRealFFT(status, &plan, noisef, noiset, npoint);
  KGLAbortIfError(status);
  
  /* read noise data */
  int nfreq=(npoint/2+1);

  for(int i=1; i<nfreq; i++){
    // FFT factor
    noisef[i] = noisef[i] / sqrt(Sn[i]) / fs;
    //printf("%e  %e  %e\n", i/(npoint/fs), cabs(noisef[i]), cabs(Sn[i]));
  }
  
  /* Get random noise data in timedomain, using IFFT*/
  KGLReverseRealFFT( status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  // IFFT factor
  for(int i=0; i<npoint; i++){
    noiset[i] = noiset[i] / (npoint/fs);
  }
}



void noise_read(double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
		double *sf7, double *sf8, double *sf9, double *sf10,
		double *Sn,
		int n,/**< (in) length of data in time domain */
		double fs){

  KGLStatus *status = KGLCreateStatus();

  FILE *fp=NULL;
  int k=0;  
  int nintmp = 1002;
  int nfreq = (n/2+1);
  double *freqin  = NULL;
  double *freqout = NULL;
  KGLCalloc(freqin,  nintmp,double,status);
  KGLCalloc(freqout, nfreq,double,status);

  double df=fs/n;
  for(k=0;k<nfreq;k++){
    freqout[k]=k*df;
  }

  double *Snin1=NULL;
  KGLCallocAbortIfError(Snin1 , nintmp,double,status);
  double *Snin2=NULL;
  KGLCallocAbortIfError(Snin2 , nintmp,double,status);
  double *Snin3=NULL;
  KGLCallocAbortIfError(Snin3 , nintmp,double,status);
  double *Snin4=NULL;
  KGLCallocAbortIfError(Snin4 , nintmp,double,status);
  double *Snin5=NULL;
  KGLCallocAbortIfError(Snin5 , nintmp,double,status);
  double *Snin6=NULL;
  KGLCallocAbortIfError(Snin6 , nintmp,double,status);
  double *Snin7=NULL;
  KGLCallocAbortIfError(Snin7 , nintmp,double,status);
  double *Snin8=NULL;
  KGLCallocAbortIfError(Snin8 , nintmp,double,status);
  double *Snin9=NULL;
  KGLCallocAbortIfError(Snin9 , nintmp,double,status);
  double *Snin10=NULL;
  KGLCallocAbortIfError(Snin10 , nintmp,double,status);

  char fname[128]="";
  char *kgl_noise_dir = getenv("KGL_NOISEDIR");
  sprintf(fname,"%s/mod4_noise.txt",kgl_noise_dir);
  if ((fp = fopen(fname, "r")) == NULL) {
    fprintf(stderr, "file open error!!\n");
    exit(1);
  }
  int i=0;
  while( fscanf(fp, "%le %le %le %le %le  %le %le %le %le %le  %le",
		&freqin[i],
		&Snin1[i], &Snin2[i], &Snin3[i], &Snin4[i], &Snin5[i], 
		&Snin6[i], &Snin7[i], &Snin8[i], &Snin9[i], &Snin10[i]
		) !=EOF){
    //printf("%le  %le\n", freqin[i], Snin1[i]);
    i++;
  }
  fclose(fp);
  int num=i; // # of input data
  
  KGLLinearInterpolation(status,sf1,freqout,Snin1,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf2,freqout,Snin2,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf3,freqout,Snin3,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf4,freqout,Snin4,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf5,freqout,Snin5,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf6,freqout,Snin6,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf7,freqout,Snin7,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf8,freqout,Snin8,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf9,freqout,Snin9,freqin,nfreq,num);
  KGLLinearInterpolation(status,sf10,freqout,Snin10,freqin,nfreq,num);
  
  for(i=0; i<nfreq; i++){
    sf1[i] = sf1[i]*sf1[i];
    sf2[i] = sf2[i]*sf2[i];
    sf3[i] = sf3[i]*sf3[i];
    sf4[i] = sf4[i]*sf4[i];
    sf5[i] = sf5[i]*sf5[i];
    sf6[i] = sf6[i]*sf6[i];
    sf7[i] = sf7[i]*sf7[i];
    sf8[i] = sf8[i]*sf8[i];
    sf9[i] = sf9[i]*sf9[i];
    sf10[i] = sf10[i]*sf10[i];
    Sn[i] = sqrt(sf1[i]*sf1[i] + sf2[i]*sf2[i] + sf3[i]*sf3[i] + sf4[i]*sf4[i] + sf5[i]*sf5[i] + 
		 sf6[i]*sf6[i] + sf7[i]*sf7[i] + sf8[i]*sf8[i] + sf9[i]*sf9[i] + sf10[i]*sf10[i]);
    //printf("%e  %e\n", freqout[i], Sn[i]);
  }

  /* for(i=0; i<nfreq; i++){
   *   printf("%e  %e\n", freqout[i], sf1[i]);
   * } */
  
  free(freqin);
  free(freqout);
  free(Snin1); free(Snin2); free(Snin3); free(Snin4); free(Snin5); 
  free(Snin6); free(Snin7); free(Snin8); free(Snin9); free(Snin10); 
  KGLDestroyStatus(status);

}


int generateAllDataFine(gsl_rng *rnd, KGLRealFFTPlan *plan,
			double f_m, double a_m, double lambda,
			double g_factor, double duration, double fs,
			double *noiset_seis, double *data_dx, double *data_h_sc,
			double *noiset, 
			cdouble *noisef_seis, cdouble *noisef,
			double *Sn,
			double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
			double *sf7, double *sf8, double *sf9, double *sf10){
  
  KGLStatus *status = KGLCreateStatus();

  int npoint = fs * duration;
  int nfreq=(npoint/2+1); 
  double df=fs/npoint;

  /* generate seismic noise */
  for(int i=0; i<nfreq; i++){
    double f = i/duration;
    if(f>FRMIN){ // FRMIN=5.0Hz
      double sig = A_SEISMIC/sqrt(fs/npoint)/2.0 / f / f;
      noisef_seis[i] = gsl_ran_gaussian(rnd,sig) + I*gsl_ran_gaussian(rnd, sig);
    }else{
      /* double sig = A_SEISMIC/sqrt(fs/npoint)/2.0 / f / f;
       * noisef_seis[i] = gsl_ran_gaussian(rnd,sig) + I*gsl_ran_gaussian(rnd, sig); */
      noisef_seis[i] = 0 + I*0;
    }
  }
  KGLReverseRealFFT( status, &plan, noiset_seis, noisef_seis, npoint);
  KGLAbortIfError(status);  

  for(int i=0; i<npoint; i++){
    double t = i/fs;
    data_dx[i]  = a_m * sin(2.0*KGL_PI*f_m * t) * exp(-t/TAU) + noiset_seis[i]*df;
    // FFT factor 
  }
  
  /* randomize initial phase */
  double phase_x_0 = 2 * KGL_PI * gsl_rng_uniform (rnd);
  for(int i=0; i<npoint; i++){
    data_h_sc[i] = g_factor * sin (4.0*KGL_PI/lambda * (data_dx[i]) + phase_x_0);
  }

  /* S_i(f) => randomized spectrum */
  /* Get random noise data in timedomain, using IFFT*/
  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf1, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] = noiset[i] * df; // IIFT factor
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf2, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf3, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf4, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf5, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf6, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf7, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf8, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf9, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, noisef, npoint, sf10, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noiset, noisef, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noiset[i] * df;
  }

  KGLDestroyStatus(status);
  return 0;
}



int generateAllDataFineWhite(gsl_rng *rnd, KGLRealFFTPlan *plan,
			     double f_m, double a_m, double lambda,
			     double g_factor, double duration, double fs,
			     double *noiset_seis, double *data_dx, double *data_h_sc,
			     double *noiset, 
			     cdouble *noisef_seis, cdouble *noisef,
			     double *Sn,
			     double *white_noiset,
			     double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
			     double *sf7, double *sf8, double *sf9, double *sf10){
  generateAllDataFine(rnd,
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
		      sf1, sf2, sf3, sf4, sf5,
		      sf6, sf7, sf8, sf9, sf10);
  KGLStatus *status = KGLCreateStatus();
  int npoint = fs * duration;  
  
  for(int i=0; i<npoint; i++){
    white_noiset[i] = noiset[i] + data_h_sc[i];
    noiset[i] = noiset[i] + data_h_sc[i];
  }
  whiten_data(status, white_noiset, npoint, fs, noisef, Sn, plan);  
  return 0;
}




int generateAllDataFineOutput(gsl_rng *rnd, KGLRealFFTPlan *plan,
			      double f_m, double a_m, double lambda,
			      double g_factor, double duration, double fs,
			      double *noiset_seis, double *data_dx, double *data_h_sc,
			      double *noiset, 
			      cdouble *noisef_seis, cdouble *noisef,
			      double *Sn,
			      double *white_noiset,
			      double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
			      double *sf7, double *sf8, double *sf9, double *sf10){
  
  KGLStatus *status = KGLCreateStatus();

  int npoint = fs * duration;
  int nfreq=(npoint/2+1); 
  double df=fs/npoint;

  /* generate seismic noise */
  for(int i=0; i<nfreq; i++){
    double f = i/duration;
    if(f>FRMIN){ // FRMIN=5.0Hz
      double sig = A_SEISMIC/sqrt(fs/npoint)/2.0 / f / f;
      noisef_seis[i] = gsl_ran_gaussian(rnd,sig) + I*gsl_ran_gaussian(rnd, sig);
    }else{
      /* double sig = A_SEISMIC/sqrt(fs/npoint)/2.0 / f / f;
       * noisef_seis[i] = gsl_ran_gaussian(rnd,sig) + I*gsl_ran_gaussian(rnd, sig); */
      noisef_seis[i] = 0 + I*0;
    }
  }
  KGLReverseRealFFT( status, &plan, noiset_seis, noisef_seis, npoint);
  KGLAbortIfError(status);  

  for(int i=0; i<npoint; i++){
    double t = i/fs;
    data_dx[i]  = a_m * sin(2.0*KGL_PI*f_m * t) * exp(-t/TAU) + noiset_seis[i]*df; //IFFT factor
  }
  
  /* randomize initial phase */
  double phase_x_0 = 2 * KGL_PI * gsl_rng_uniform (rnd);
  for(int i=0; i<npoint; i++){
    data_h_sc[i] = g_factor * sin (4.0*KGL_PI/lambda * (data_dx[i]) + phase_x_0);
  }


  /* read data here */
  cdouble *sfr1=NULL;
  KGLCallocAbortIfError(sfr1 , nfreq,cdouble,status);
  cdouble *sfr2=NULL;
  KGLCallocAbortIfError(sfr2 , nfreq,cdouble,status);
  cdouble *sfr3=NULL;
  KGLCallocAbortIfError(sfr3 , nfreq,cdouble,status);
  cdouble *sfr4=NULL;
  KGLCallocAbortIfError(sfr4 , nfreq,cdouble,status);
  cdouble *sfr5=NULL;
  KGLCallocAbortIfError(sfr5 , nfreq,cdouble,status);
  cdouble *sfr6=NULL;
  KGLCallocAbortIfError(sfr6 , nfreq,cdouble,status);
  cdouble *sfr7=NULL;
  KGLCallocAbortIfError(sfr7 , nfreq,cdouble,status);
  cdouble *sfr8=NULL;
  KGLCallocAbortIfError(sfr8 , nfreq,cdouble,status);
  cdouble *sfr9=NULL;
  KGLCallocAbortIfError(sfr9 , nfreq,cdouble,status);
  cdouble *sfr10=NULL;
  KGLCallocAbortIfError(sfr10 , nfreq,cdouble,status);

  double *noisetmp=NULL;
  KGLCallocAbortIfError(noisetmp, npoint, double,status);


  /* S_i(f) => randomized spectrum */
  /* Get random noise data in timedomain, using IFFT*/
  KGLGenerateGaussNoiseGSL(status, sfr1, npoint, sf1, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr1, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] = noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr2, npoint, sf2, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr2, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr3, npoint, sf3, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr3, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr4, npoint, sf4, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr4, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr5, npoint, sf5, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr5, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr6, npoint, sf6, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr6, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr7, npoint, sf7, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr7, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr8, npoint, sf8, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr8, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr9, npoint, sf9, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr9, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }

  KGLGenerateGaussNoiseGSL(status, sfr10, npoint, sf10, 5, fs, rnd);
  KGLAbortIfError(status);
  KGLReverseRealFFT(status, &plan, noisetmp, sfr10, npoint);
  KGLAbortIfError(status);
  for(int i=0; i<npoint; i++){
    noiset[i] += noisetmp[i] * df;
  }


  /* output check of simulation noise in time domain */
  char fname[128]="";
  FILE *fp=NULL;
  sprintf(fname, "./dat/data_sf_fs%.0f_f_m%.0f_a%.1e_duration_%.1f_tau%.1e.txt", fs, f_m, a_m, duration, TAU);
  fp = fopen(fname,"w");
  for(int i=0; i<nfreq; i++){
    fprintf(fp, "%e %e %e %e %e  %e %e %e %e %e  %e\n",
 	    i/(npoint/fs),
	    cabs(sfr1[i]), cabs(sfr2[i]), cabs(sfr3[i]), cabs(sfr4[i]), cabs(sfr5[i]),
	    cabs(sfr6[i]), cabs(sfr7[i]), cabs(sfr8[i]), cabs(sfr9[i]), cabs(sfr10[i])
	    //sf1[i], (sf2[i]), (sf3[i]), (sf4[i]), (sf5[i]),
	    //sf6[i], (sf7[i]), (sf8[i]), (sf9[i]), (sf10[i])
	    );
  }
  
  for(int i=0; i<npoint; i++){
    white_noiset[i] = noiset[i] + data_h_sc[i];
    noiset[i] = noiset[i] + data_h_sc[i];
  }
  whiten_data(status, white_noiset, npoint, fs, noisef, Sn, plan);  

  
  free(noisetmp);
  KGLDestroyStatus(status);
  return 0;
}
