#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int fir_filter (double *output,  double *input, unsigned inputlen, double fir_coeff[], double fir_buffer[], unsigned *index, unsigned length){

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

