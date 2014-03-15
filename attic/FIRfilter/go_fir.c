
#include <stdio.h>

# define FIR_LEN 1000
double x,y;
double fir_buffer[2*FIR_LEN];
double fir_coeff[FIR_LEN];
unsigned fir_ix;
unsigned temp;

double fir_filter(double, double[], double[], unsigned*, unsigned);

int main (void){
    printf("Start program.\n");
    // initialize FIR Filter
    for (temp=0;temp<FIR_LEN;temp++){
        fir_coeff[temp]=11.0;
        fir_buffer[temp]=0.0;
        fir_buffer[temp+FIR_LEN]=0.0;
    }
    // impulse response of echo
    fir_coeff[490]=1;
    fir_coeff[440]=0.6;
    fir_coeff[200]=0.3;
    fir_coeff[130]=0.6;
    fir_coeff[1]=0.4;
    fir_ix=0;

    //main part
    x = 5;
    y = fir_filter(x, fir_coeff, fir_buffer, &fir_ix, FIR_LEN);
    printf("Filter output is %f\n", y);
    return 1;

}


