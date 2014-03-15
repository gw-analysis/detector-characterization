#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int fir_filter (double *output,  double *input, unsigned inputlen, double fir_coeff[], double fir_buffer[], unsigned *index, unsigned length){

    unsigned idx, oidx;
    //const int Nin = sizeof input /sizeof input[0];
    //output = (double *)malloc(sizeof(double)*Nin);
    //memset(output, 0, sizeof(double)*Nin);

    for (oidx=0;oidx<inputlen;oidx++){

        fir_buffer[*index]        = input[oidx];
        fir_buffer[*index+length] = input[oidx];

        *index=*index+1;
        if(*index==length) *index=0;

        //utput[0]=0;
        for (idx=0;idx<length;idx++){
            output[oidx] = output[oidx]
                + fir_coeff[idx]*fir_buffer[*index+idx];
        }
        //*index=*index+1;

    }

    return 1;
}

