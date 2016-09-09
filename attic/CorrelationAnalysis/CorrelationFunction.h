
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_cdf.h>

#include <kagali/KGLStdlib.h>
#include <kagali/KGLNoisePSD.h>
#include <kagali/RealFFT.h>
#include <kagali/KGLLinearAlgebra.h>


#define MAX_PTS 100000
#define MAX_DIG 20
#define MAX_LEN MAX_DIG*MAX_PTS
#define NUM_SIM 100

double getrhoPearson(double val1[],double val2[],int len);
void shuffle(int ran[],int len);
double getrhoDistantCor(double val1[],double val2[],int len);

int generateAllData(gsl_rng *rnd, KGLRealFFTPlan *plan,
		    double f_m, double a_m, double lambda,
		    double g_factor, double duration, double fs,
		    double *noiset_seis, double *data_dx, double *data_h_sc,
		    double *noiset, 
		    cdouble *noisef_seis, cdouble *noisef, double *Sn);

int generateAllDataWhite(gsl_rng *rnd, KGLRealFFTPlan *plan,
			 double f_m, double a_m, double lambda,
			 double g_factor, double duration, double fs,
			 double *noiset_seis, double *data_dx, double *data_h_sc,
			 double *noiset, 
			 cdouble *noisef_seis, cdouble *noisef, double *Sn,
			 double *white_noiset);


void whiten_data(KGLStatus *status,
		 double *noiset,
		 int npoint,
		 double fs,
		 cdouble *noisef,
		 double *Sn,
		 KGLRealFFTPlan *plan);

void noise_read(double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
		double *sf7, double *sf8, double *sf9, double *sf10,
		double *Sn,
		int n,/**< (in) length of data in time domain */
		double fs);


int generateAllDataFine(gsl_rng *rnd, KGLRealFFTPlan *plan,
			double f_m, double a_m, double lambda,
			double g_factor, double duration, double fs,
			double *noiset_seis, double *data_dx, double *data_h_sc,
			double *noiset, 
			cdouble *noisef_seis, cdouble *noisef,
			double *Sn,
			double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
			double *sf7, double *sf8, double *sf9, double *sf10);

int generateAllDataFineWhite(gsl_rng *rnd, KGLRealFFTPlan *plan,
			     double f_m, double a_m, double lambda,
			     double g_factor, double duration, double fs,
			     double *noiset_seis, double *data_dx, double *data_h_sc,
			     double *noiset, 
			     cdouble *noisef_seis, cdouble *noisef,
			     double *Sn,
			     double *white_noiset,
			     double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
			     double *sf7, double *sf8, double *sf9, double *sf10);

int generateAllDataFineOutput(gsl_rng *rnd, KGLRealFFTPlan *plan,
			      double f_m, double a_m, double lambda,
			      double g_factor, double duration, double fs,
			      double *noiset_seis, double *data_dx, double *data_h_sc,
			      double *noiset, 
			      cdouble *noisef_seis, cdouble *noisef,
			      double *Sn,
			      double *white_noiset,
			      double *sf1, double *sf2, double *sf3, double *sf4, double *sf5, double *sf6,
			      double *sf7, double *sf8, double *sf9, double *sf10);
