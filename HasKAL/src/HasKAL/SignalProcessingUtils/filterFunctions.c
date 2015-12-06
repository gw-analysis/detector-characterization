
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "filterFunctions.h"

#ifndef M_PI
#define M_PI           3.14159265358979323846
#endif



int iir_filter (double *input, unsigned inputlen, double num_coeff[], double denom_coeff[], unsigned filterlen, double *output){

    unsigned inputidx, k;
    double y, init_coeff[filterlen];

    //-- initialize the delay registers
    for (k=0;k<filterlen;k++){
        init_coeff[k] = 0.0;
    }

    //-- main part
    iir_filter_core (input, inputlen, num_coeff, denom_coeff, filterlen, init_coeff, output);

    return 1;
}


int iir_filter_init (double *input, unsigned inputlen, double num_coeff[], double denom_coeff[], double init[], unsigned filterlen, double *output){

    unsigned inputidx, k;
    double y, init_coeff[filterlen];

    //-- initialize the delay registers
    for (k=0;k<filterlen;k++){
        init_coeff[k] = init[k];
    }

    //-- main part
    iir_filter_core (input, inputlen, num_coeff, denom_coeff, filterlen, init_coeff, output);

    return 1;
}


int iir_filter_core (double *input, unsigned inputlen, double num_coeff[], double denom_coeff[], unsigned filterlen, double init_coeff[], double *output){

    unsigned inputidx, k;
    double y, Reg[filterlen];

    //-- initialize the delay registers
    for (k=0;k<filterlen;k++){
        Reg[k] = init_coeff[k];
    }

    for (inputidx=0;inputidx<inputlen;inputidx++){
        //-- Shift the delay register values.
        for (k=filterlen-1;k>0;k--) {
            Reg[k] = Reg[k-1];
        }

        //-- denominator part
        Reg[0] = input[inputidx];
        for (k=1;k<filterlen;k++) {
            Reg[0] -= denom_coeff[k] * Reg[k];
        }

        //-- numerator part
        y = 0;
        for (k=0;k<filterlen;k++) {
            y += num_coeff[k] * Reg[k];
        }
        output[inputidx] = y;
    }
    return 1;
}


int fir_filter (double *input, unsigned inputlen, double fir_coeff[], unsigned filterlen, double *output){

    double fir_buffer[2*filterlen];
    unsigned temp;
    unsigned fir_ix;

    //-- initialize ring buffer
    for (temp=0;temp<filterlen;temp++){
        fir_buffer[temp]=0.0;
        fir_buffer[temp+filterlen]=0.0;
    }
    //-- initialize output
    for (temp=0;temp<inputlen;temp++){
        output[temp]=0.0;
    }
    //-- set input index
    fir_ix = 0;

    //-- main part
    fir_filter_core(input,  inputlen,  fir_coeff,  fir_buffer,  &fir_ix,  filterlen, output);


    return 1;
}


int fir_filter_init (double *input, unsigned inputlen, double fir_coeff[], double init[], unsigned filterlen, double *output){

    double fir_buffer[2*filterlen];
    unsigned temp;
    unsigned fir_ix;

    //-- initialize ring buffer
    for (temp=0;temp<filterlen;temp++){
        fir_buffer[temp]=init[temp];
        fir_buffer[temp+filterlen]=0.0;
    }
    //-- initialize output
    for (temp=0;temp<inputlen;temp++){
        output[temp]=0.0;
    }
    //-- set input index
    fir_ix = 0;

    //-- main part
    fir_filter_core(input,  inputlen,  fir_coeff,  fir_buffer,  &fir_ix,  filterlen, output);


    return 1;
}



int fir_filter_core (double *input, unsigned inputlen, double fir_coeff[], double fir_buffer[], unsigned *index, unsigned filterlen, double *output){

    unsigned idx, oidx;

    for (oidx=0;oidx<inputlen;oidx++){

        fir_buffer[*index]           = input[oidx];
        fir_buffer[*index+filterlen] = input[oidx];
        *index = *index+1;
        if(*index==filterlen) *index=0;

        for (idx=0;idx<filterlen;idx++){
            output[oidx] = output[oidx]
                + fir_coeff[idx]*fir_buffer[*index+idx];
        }
    }
    return 1;
}


double goertzel (double *Samples, int N, double fs, double freq)
{
 int j;
 double Reg[3];        // 3 shift registers
 double CosVal, Mag, Omega;
 Omega = freq/(fs/2);
 Reg[1] = Reg[2] = 0.0;

 CosVal = 2.0 * cos(M_PI * Omega );
 for (j=0; j<N; j++)
  {
   Reg[0] = Samples[j] + CosVal * Reg[1] - Reg[2];
   Reg[2] = Reg[1];  // Shift the values.
   Reg[1] = Reg[0];
  }
 Mag = Reg[2] * Reg[2] + Reg[1] * Reg[1] - CosVal * Reg[1] * Reg[2];

 if(Mag > 0.0)Mag = sqrt(Mag);
 else Mag = 1.0E-42;   // To prevent a problem in dB()

 return(Mag);
}


int filtfilt (double *input, unsigned inputlen, double num_coeff[], double denom_coeff[], unsigned filterlen, double *output){

    unsigned inputidx, k, i;
    double y, init_coeff[filterlen];

    //-- initialize the delay registers
    for (k=0;k<filterlen;k++){
        init_coeff[k] = 0.0;
    }

    //-- forward filtering
    iir_filter_core (input, inputlen, num_coeff, denom_coeff, filterlen, init_coeff, output);

    //-- reverse filtering
    //-- input : reversed output
    for (i=0;i<inputlen;i++){
        input[i] = output[inputlen-i-1];
    }
    //--filtering again
    iir_filter_core (input, inputlen, num_coeff, denom_coeff, filterlen, init_coeff, output);
    //-reverse output
    for (i=0;i<inputlen;i++){
        input[i] = output[inputlen-i-1];
    }
    for (i=0;i<inputlen;i++){
        output[i] = input[inputlen-i-1];
    }


    return 1;
}


int filtfilt_init (double *input, unsigned inputlen, double num_coeff[], double denom_coeff[], double init[], unsigned filterlen, double *output){

    unsigned inputidx, k, i;
    double y, init_coeff[filterlen];

    //-- initialize the delay registers
    for (k=0;k<filterlen;k++){
        init_coeff[k] = init[k];
    }

    //-- forward filtering
    iir_filter_core (input, inputlen, num_coeff, denom_coeff, filterlen, init_coeff, output);

    //-- reverse filtering
    //-- input : reversed output
    for (i=0;i<inputlen;i++){
        input[i] = output[inputlen-i-1];
    }
    //--filtering again
    iir_filter_core (input, inputlen, num_coeff, denom_coeff, filterlen, init_coeff, output);
    //-reverse output
    for (i=0;i<inputlen;i++){
        input[i] = output[inputlen-i-1];
    }
    for (i=0;i<inputlen;i++){
        output[i] = input[inputlen-i-1];
    }


    return 1;
}


void sosfilter (double *input, unsigned inputlen, double *num_coeff0, double *num_coeff1,double *num_coeff2, double *denom_coeff0, double *denom_coeff1, double *denom_coeff2, unsigned nsect, double *output)
{
 double y;
 int j, k;
 double RegX1[nsect], RegX2[nsect], RegY1[nsect], RegY2[nsect];

 for(j=0; j<nsect; j++)
  {
   RegX1[j] = 0.0;
   RegX2[j] = 0.0;
   RegY1[j] = 0.0;
   RegY2[j] = 0.0;
  }

 for(j=0; j<inputlen; j++)
  {
   y = sosform1(0, input[j], num_coeff0, num_coeff1, num_coeff2, denom_coeff0, denom_coeff1, denom_coeff2, RegX1, RegX2, RegY1, RegY2);
   for(k=1; k<nsect; k++)
  {
   y = sosform1(k, y, num_coeff0, num_coeff1, num_coeff2, denom_coeff0, denom_coeff1, denom_coeff2, RegX1, RegX2, RegY1, RegY2);
  }
   output[j] = y;
  }
}


void sosfilter_init (double *input, unsigned inputlen, double *num_coeff0, double *num_coeff1,double *num_coeff2, double *denom_coeff0, double *denom_coeff1, double *denom_coeff2, unsigned nsect, double *init1, double *init2, double *output)
{
 double y;
 int j, k;
 double RegX1[nsect], RegX2[nsect], RegY1[nsect], RegY2[nsect];

 for(j=0; j<nsect; j++)
  {
   RegX1[j] = init1[j];
   RegX2[j] = init2[j];
   RegY1[j] = init1[j];
   RegY2[j] = init2[j];
  }

 for(j=0; j<inputlen; j++)
  {
   y = sosform1(0, input[j], num_coeff0, num_coeff1, num_coeff2, denom_coeff0, denom_coeff1, denom_coeff2, RegX1, RegX2, RegY1, RegY2);
   for(k=1; k<nsect; k++)
  {
   y = sosform1(k, y, num_coeff0, num_coeff1, num_coeff2, denom_coeff0, denom_coeff1, denom_coeff2, RegX1, RegX2, RegY1, RegY2);
  }
   output[j] = y;
  }
}


double sosform1(int k, double x, double *num_coeff0, double *num_coeff1, double *num_coeff2, double *denom_coeff0, double *denom_coeff1, double *denom_coeff2, double *RegX1, double *RegX2, double *RegY1, double *RegY2)
{
 double y, point;

 point = x * num_coeff0[k] + num_coeff1[k] * RegX1[k] + num_coeff2[k] * RegX2[k];
 y = denom_coeff0[k] * point - denom_coeff1[k] * RegY1[k] - denom_coeff2[k] * RegY2[k];

 RegX2[k] = RegX1[k];
 RegX1[k] = x;
 RegY2[k] = RegY1[k];
 RegY1[k] = y;

 return(y);
}


void sosstatespace (double *x, unsigned inputlen, double *A, double *B, double *C, double D, double x01, double x02, double *output)
{
 int k;
 double x11, x12;
 for (k=0;k<inputlen;k++)
  {
   if (k==0) x11=x01; x12=x02;
   output[k] = C[0]*x11+C[1]*x12 + D*x[k];
   x11 = A[0]*x11+A[1]*x12 + B[0]*x[k];
   x12 = A[2]*x11+A[3]*x12 + D*x[k];
  }
}


