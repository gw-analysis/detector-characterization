#include <stdio.h>
#include <gsl/gsl_spline.h> //interp
#include <gsl/gsl_interp.h> //interp
#include <gsl/gsl_sf_erf.h> //error function

int main(void)
{
  int index;
  int nn=400;
  int nnn=200;
  double errx[nn], erry[nn];
  double xx[nnn],yy[nnn];

  for(index=0;index<nn;index++)
    {
      errx[index]=-1.0*(double)index*0.01+2.0;
      erry[index]=gsl_sf_erfc(errx[index]);
      //printf("%lf %lf\n",errx[index],erry[index]);
    }
  
  gsl_interp_accel *acc = gsl_interp_accel_alloc ();
  const gsl_interp_type *t = gsl_interp_cspline_periodic;
  gsl_spline *spline = gsl_spline_alloc (t, nn);
  gsl_spline_init (spline, erry, errx, nn);

  for(index=1;index<nnn;index++)
    {
      xx[index]=(double)index*0.01;
      yy[index]=gsl_spline_eval(spline, xx[index], acc);
      printf("%lf %lf\n",xx[index],yy[index]);
    }
  
  return 0;

}

