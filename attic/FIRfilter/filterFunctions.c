
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//-- prototype
int fir_filter_core(double*,  double*,  unsigned,  double[],  double[],  unsigned*,  unsigned);
int fir_filter (double*, double*, unsigned, double[], unsigned);
int iir_filter (double*,  double*,  unsigned,  double[],  double[], unsigned);



int iir_filter (double *output, double *input, unsigned inputlen, double num_coeff[], double denom_coeff[], unsigned filterlen){

    unsigned inputidx, k;
    double y, Reg[filterlen];

    //-- initialize the delay registers
    for (k=0;k<filterlen;k++){
        Reg[k] = 0.0;
    }

    for (inputidx=0;inputidx<inputlen;inputidx++){
        //-- Shift the delay register values.
        for (k=filterlen;k>0;k--) {
            Reg[k] = Reg[k-1];
        }

        //-- denominator part
        Reg[0] = input[inputidx];
        for (k=1;k<=filterlen;k++) {
            Reg[0] -= denom_coeff[k] * Reg[k];
        }

        //-- numerator part
        y = 0;
        for (k=0;k<=filterlen;k++) {
            y += num_coeff[k] * Reg[k];
        }
        output[inputidx] = y;
    }
    return 1;
}



int fir_filter (double *output, double *input, unsigned inputlen, double fir_coeff[], unsigned filterlen){

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
    fir_filter_core(output,  input,  inputlen,  fir_coeff,  fir_buffer,  &fir_ix,  filterlen);


    return 1;
}



int fir_filter_core (double *output,  double *input, unsigned inputlen, double fir_coeff[], double fir_buffer[], unsigned *index, unsigned length){

    unsigned idx, oidx;

    for (oidx=0;oidx<inputlen;oidx++){

        fir_buffer[*index]        = input[oidx];
        fir_buffer[*index+length] = input[oidx];

        *index=*index+1;
        if(*index==length) *index=0;

        for (idx=0;idx<length;idx++){
            output[oidx] = output[oidx]
                + fir_coeff[idx]*fir_buffer[*index+idx];
        }

    }

    return 1;
}

