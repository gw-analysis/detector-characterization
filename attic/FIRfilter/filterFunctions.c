#include <stdio.h>

double fir_filter (double input, double fir_coeff[], double fir_buffer[], unsigned *index, unsigned length){

    double output;
    unsigned idx;

    fir_buffer[*index]        = input;
    fir_buffer[*index+length] = input;

    *index=*index+1;
    if(*index==length) *index=0;

    output = 0;
    for (idx=0;idx<length;idx++){
        output = output + fir_coeff[idx]*fir_buffer[*index+idx];
    }

//    printf("Filter output is %f\n", output);
    return output;
}

