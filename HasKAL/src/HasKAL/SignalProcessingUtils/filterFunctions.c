
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "filterFunctions.h"
//-- prototype
//int fir_filter_core(double*,  unsigned,  double[],  double[],  unsigned*,  unsigned, double*);
//int fir_filter (double*, unsigned, double[], unsigned,  double*);
//int iir_filter (double*,  unsigned,  double[],  double[], unsigned, double*);
//


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
    //-- set input index
    fir_ix = 0;

    //-- main part
    fir_filter_core(input,  inputlen,  fir_coeff,  fir_buffer,  &fir_ix,  filterlen, output);


    return 1;
}



int fir_filter_core (double *input, unsigned inputlen, double fir_coeff[], double fir_buffer[], unsigned *index, unsigned filterlen, double *output){

    unsigned idx, oidx;

    for (oidx=0;oidx<inputlen;oidx++){

        fir_buffer[*index]        = input[oidx];
        fir_buffer[*index+filterlen] = input[oidx];

        if(*index==filterlen) *index=0;

        for (idx=0;idx<filterlen;idx++){
            output[oidx] = output[oidx]
                + fir_coeff[idx]*fir_buffer[*index+filterlen-idx];
        }
        *index=*index+1;


    }

    return 1;
}

