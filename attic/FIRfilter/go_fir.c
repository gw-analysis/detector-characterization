
#include <stdio.h>

# define FIR_LEN 4
double x[]={1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21};
double y[]={0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0};
double fir_buffer[2*FIR_LEN];
double fir_coeff[FIR_LEN];
unsigned fir_ix;
unsigned temp;
unsigned inputlen = 11;

double fir_filter(double*, double*, unsigned, double[], double[], unsigned*, unsigned);

int main (void){

    printf("Start program.\n");

    // initialize FIR Filter
    for (temp=0;temp<FIR_LEN;temp++){
        fir_coeff[temp]=11.0;
        fir_buffer[temp]=0.0;
        fir_buffer[temp+FIR_LEN]=0.0;
    }

    fir_coeff[3]=0;
    fir_coeff[2]=0;
    fir_coeff[1]=0;
    fir_coeff[0]=1;
    fir_ix=0;

    //main part
    fir_filter(y, x, inputlen, fir_coeff, fir_buffer, &fir_ix, FIR_LEN);

    printf("Filter output is %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f\n", y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7], y[8], y[9], y[10], y[11]);

    return 1;

}


