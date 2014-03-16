
#include <stdio.h>

# define FIR_LEN 5
double x[]={1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21};
double y[]={0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0};
unsigned inputlen = 11;

unsigned filterlen = FIR_LEN;
double num_coeff[FIR_LEN];
double denom_coeff[FIR_LEN];


int iir_filter (double*,  double*,  unsigned,  double[],  double[], unsigned);


int main (void){

    printf("Start program.\n");


    num_coeff[4] = 9.7E-3;
    num_coeff[3] = 1.8E-2;
    num_coeff[2] = 2.4E-2;
    num_coeff[1] = 1.8E-2;
    num_coeff[0] = 9.7E-3;

    denom_coeff[4] = 1.8E-1;
    denom_coeff[3] = -1.0E0;
    denom_coeff[2] = 2.2E0;
    denom_coeff[1] = -2.3E0;
    denom_coeff[0] = 1.0E0;

    //main part
    iir_filter(y, x, inputlen, num_coeff, denom_coeff, filterlen);

    printf("Filter output is %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f\n", y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7], y[8], y[9], y[10], y[11]);

    return 1;

}


