// filterX.c
// INPUT:
//   b, a: Filter parameters as DOUBLE vectors. If the vectors have different
//      lengths, the shorter one is padded with zeros.
//   X: Signal as DOUBLE vector. The signal is filtered along
//      the first dimension (!even if X is a row vector!).
//   Z: Initial conditions as DOUBLE array. The size must be:
//        [(Order) - 1, SIZE(X,2), ..., SIZE(X, NDIMS(X))]
//   Reverse: The signal is processed in reverse order, if this is TRUE or
//      'reverse'. b, a and Z are not affected - see examples.
//
// OUTPUT:
//   Y: Filtered signal with the same size and type as X.
//   Z: Final conditions as DOUBLE array.
//
// NOTES:
//   - The output equals the output of FILTER exactly for DOUBLEs.
//   - The parameters [a] and [b] are normalized to the 1st element of a, if
//     a(1) differs from 1.0.
//   - This function filters along the 1st dimension only. Use FilterM as
//     wrapper to process other dimensions also.
//
// COMPILATION:
//   gcc -O filter.c
// Consider C99 comments on Linux with GCC:
//   gcc -O CFLAGS="\$CFLAGS -std=c99" filter.c
// this function is imported from http://jp.mathworks.com/matlabcentral/fileexchange/32261-filterm


// Headers: --------------------------------------------------------------------
#include <stdlib.h>
#include <float.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include "filterX.h"

// Definitions: ----------------------------------------------------------------
#ifndef bool         
#define bool int
#endif
#ifndef TRUE            
#define TRUE 1
#endif
#ifndef FALSE           
#define FALSE 0
#endif

// Limit number of dimensions of the input - this saves 2% computing time if
// the signal is tiny (e.g. [16 x 1]):
#define MAX_NDIMS 32

// Disable the /fp:precise flag to increase the speed on MSVC compiler:
#ifdef _MSC_VER
#pragma float_control(except, off)    // disable exception semantics
#pragma float_control(precise, off)   // disable precise semantics
#pragma fp_contract(on)               // enable contractions
// #pragma fenv_access(off)           // disable fpu environment sensitivity
#endif


// Prototypes: -----------------------------------------------------------------
void CoreDoubleN(double *X, int MX, int NX, double *a, double *b,
        int order, double *Z, double *Y);
void CoreDouble2(double *X, int MX, int NX, double *a, double *b,
        double *Z, double *Y);
void CoreDouble3(double *X, int MX, int NX, double *a, double *b,
        double *Z, double *Y);
void CoreDouble4(double *X, int MX, int NX, double *a, double *b,
        double *Z, double *Y);
void CoreDouble5(double *X, int MX, int NX, double *a, double *b,
        double *Z, double *Y);
void CoreDouble6(double *X, int MX, int NX, double *a, double *b,
        double *Z, double *Y);
void CoreDouble7(double *X, int MX, int NX, double *a, double *b,
        double *Z, double *Y);

void CoreSingleN(float *X, int MX, int NX, double *a, double *b,
        int order, double *Z, float *Y);
void CoreSingle2(float *X, int MX, int NX, double *a, double *b,
        double *Z, float *Y);
void CoreSingle3(float *X, int MX, int NX, double *a, double *b,
        double *Z, float *Y);
void CoreSingle4(float *X, int MX, int NX, double *a, double *b,
        double *Z, float *Y);
void CoreSingle5(float *X, int MX, int NX, double *a, double *b,
        double *Z, float *Y);
void CoreSingle6(float *X, int MX, int NX, double *a, double *b,
        double *Z, float *Y);
void CoreSingle7(float *X, int MX, int NX, double *a, double *b,
        double *Z, float *Y);

void CoreDoubleNR(double *X, int MX, int NX, double *a, double *b,
        int order, double *Z, double *Y);
void CoreSingleNR(float *X, int MX, int NX, double *a, double *b,
        int order, double *Z, float *Y);

void CopySingleToDouble(double *Z, float *Zf, int N);
void NormalizeBA(double *ab, int nParam);
        
// Main function ===============================================================
// Y_out
// Z_out
// b_in：フィルタの分母係数
// nb：フィルタの数
// a_in：フィルタの分子係数
// na：フィルタの数
// X_in：時系列データ（mxn行列。n個の時系列データ。各時系列データはmサンプル）
// mX_in：時系列データのサンプル数（元のmatlabバージョンは各時系列ごとにサンプル数を変えられたがここではすべて同じ長さにしている。）
// nX_in：時系列データの数
// Xdims_in：それぞれの時系列データのサンプル数(次元数)が入った配列。このコードでは各要素（nX_in個）に同じ値が入る。
// Z_in
// Rev_in : Reverseかどうかの

void filter(double *Y_out, double* Z_out, double *b_in, int nb, double *a_in, int na, double *X_in, int mX_in, int nX_in, double *Z_in, const char *Rev_in)
{
  double *a, *b, a0;
  float  *Xf, *Yf;
  int order, nParam, MX, NX;
  bool allocate_ba = FALSE, forward = TRUE;
  int *Xdims;
  
  
  // Get dimensions of inputs:
  MX = mX_in;    // First dimension (はじめのデータのサンプル数)関数 mxGetM は、指定された配列内の行数を返します。"行" という用語は、配列内にいくつの次元が存在しても、配列の最初の次元を常に意味します。たとえば、8 x 9 x 5 x 3 の次元をもつ 4 次元配列を pm が指している場合、関数 mxGetM は 8 を返します。
  NX = nX_in;
//  NX = mX_in * (nX_in-1);    // 関数 mxGetN を呼び出して、指定された mxArray 内の列数を判断します。pm が N 次元の mxArray である場合、関数 mxGetN は 2 から N の次元の積になります。たとえば、13 x 5 x 4 x 6 の次元をもつ 4 次元の mxArray を pm が指し示している場合、関数 mxGetN は値 120 (5 x 4 x 6) を返します。ここではすべての時系列データ（合計列数nX_in）が同じ長さmX_inを持っているとする。通常はN=2の行列のため、mxGetNは行列の列数を表す。
  
  // Get a and b as vectors of the same length:
  if (na == nb) {       // Use input vectors directly, if possible:
     b      = b_in;
     a      = a_in;
     nParam = nb;
     
     allocate_ba = (bool) (a[0] != 1.0);  // Normalization is needed
     
  } else {              // na differs from nb:
     nParam      = na > nb ? na : nb;
     allocate_ba = TRUE;
  }
  order = nParam - 1;
  
  if (allocate_ba) {    // Create local copy of expanded [b] and [a]:
     // It is slightly cheaper to allocate one array only:
     if ((b = calloc(2 * nParam, sizeof(double))) == NULL) {
        fprintf(stderr,"Cannot get memory for parameters.");
     }
     a = b + nParam;    // Use 2nd half of vector [b] as [a]
     memcpy(b, b_in, nb * sizeof(double));
     memcpy(a, a_in, na * sizeof(double));
     
     // Normalize if 1st element of [a] does not equal 1.0:
     if (a[0] != 1.0) {
        NormalizeBA(b, nParam);
     }
  }
  
  // Create array for final conditions, insert value of initial conditions:
  if (nX_in > MAX_NDIMS) {
     fprintf(stderr, "Signal cannot have more than %d dimensions.", MAX_NDIMS);
  }
  for (int i=0;i<nX_in;i++) {
    Xdims[i] = mX_in; // それぞれの時系列データのサンプル数(次元数)が入った配列
  }
    
//matlabの配列は下のようになっている。こうするのが理想であるが、このコードではとりあえずすべての次元は同じサンプル数持っているとする。
//int D0[Xdims[0]];
//int D1[Xdims[1]];
//.....
//int D(nX_in-1)[Xdims[nX_in-1];
//int *D[nX_in] = { D0, D1, ...., D(nX_in-1)};
//D[5][5] = 55;

  Z_out = calloc(order * nX_in, sizeof(double));
  // Copy value from input with a conversion to DOUBLE on demand:
  memcpy(Z_out, Z_in, order * nX_in * sizeof(double));
     



  // Flag for forward processing: ----------------------------------------------
  // 'Reverse', TRUE: Process signal in reverse order:
  forward = (bool) (strcmp(Rev_in,"reverse")!=0 && strcmp(Rev_in,"Reverse")!=0);

  // Call the calulator: -------------------------------------------------------
  // Create the output array:
  Y_out = calloc(MX * nX_in, sizeof(double));

     
  if (forward) {
     // Unrolled loops for common filter lengths:
     switch (order) {
        case 1:   CoreDouble2(X_in, MX, NX, a, b, Z_out, Y_out);  break;
        case 2:   CoreDouble3(X_in, MX, NX, a, b, Z_out, Y_out);  break;
        case 3:   CoreDouble4(X_in, MX, NX, a, b, Z_out, Y_out);  break;
        case 4:   CoreDouble5(X_in, MX, NX, a, b, Z_out, Y_out);  break;
        case 5:   CoreDouble6(X_in, MX, NX, a, b, Z_out, Y_out);  break;
        case 6:   CoreDouble7(X_in, MX, NX, a, b, Z_out, Y_out);  break;
        default:  CoreDoubleN(X_in, MX, NX, a, b, order, Z_out, Y_out);
     }
  } else {  // Reverse order:
     CoreDoubleNR(X_in, MX, NX, a, b, order, Z_out, Y_out);
  }

  // Cleanup:
  if (allocate_ba) {
     free(b);       // Frees [a] implicitely!
  }
  
  return;
}



// =============================================================================
void NormalizeBA(double *ba, int nParam)
{
  // Normalize filter parameters such that a[0] is 1.0.
  double a0 = ba[nParam];
  int i = 0, f = 2 * nParam;
  
  // Catch division by zero as error:
  if (a0 == 0.0) {
     fprintf(stderr,"1st element of A cannot be 0.");
  }
        
  while (i < f) {
     ba[i++] /= a0;
  }
  
  return;
}

//  ****************************************************************************
//  ***                               DOUBLE                                 ***
//  ****************************************************************************

void CoreDoubleN(double *X, int MX, int NX, double *a, double *b,
                 int order, double *Z, double *Y)
{
  // Direct form II transposed method for general filter length.
  // Implemented as time domain difference equations.
  // INPUT:
  //   X:  Double array. Operation happens of 1st dimension.
  //   MX: Number of elements in the 1st dimension
  //   NX: Number of columns, considers mutliple dimensions.
  //   a, b: Double vector of filter parameters. Both have nParam elements.
  //       The first element of a is 1.
  //   Z:  DOUBLE array, initial conditions.
  //   nParam: Number of filter parameters, order of filter + 1.
  // OUTPUT:
  //   Z:  DOUBLE array, final conditions.
  //   Y:  Double array, allocated by the caller.
   
  double Xi, Yi;
  int i, j, R;
  
  i = 0;
  while (NX--) {                         // Next slice
     R = i + MX;                         // End of the column
     while (i < R) {
        Xi = X[i];                       // Get signal
        Yi = b[0] * Xi + Z[0];           // Filtered value
        for (j = 1; j < order; j++) {    // Update conditions
           Z[j - 1] = b[j] * Xi + Z[j] - a[j] * Yi;
        }
        Z[order - 1] = b[order] * Xi - a[order] * Yi;
        
        Y[i++] = Yi;                      // Write to output
     }
     Z += order;                          // Next condition vector
  }
  
  return;
}

// =============================================================================
void CoreDouble2(double *X, int MX, int NX, double *a, double *b,
                 double *Z, double *Y)
{
  // Filter with loop unrolled for 2 parameters (filter order 1).
  // Same input as the CoreDoubleN, but ommited [order], because it is 1.
   
  double Xi, Yi, z0, a1 = a[1];
  int i = 0, C;
  
  while (NX--) {
     z0 = Z[0];
     C  = i + MX;
     while (i < C) {
        Xi = X[i];
        Yi = b[0] * Xi + z0;
        z0 = b[1] * Xi - a1 * Yi;
        Y[i++] = Yi;
     }
     *Z++ = z0;
  }

  return;
}

// =============================================================================
void CoreDouble3(double *X, int MX, int NX, double *a, double *b,
                 double *Z, double *Y)
{
  double Xi, Yi, z0, z1, a1 = a[1], a2 = a[2];
  int i = 0, C;
  
  while (NX--) {
     z0 = Z[0];
     z1 = Z[1];
     C  = i + MX;
     while (i < C) {
        Xi = X[i];
        Yi = b[0] * Xi + z0;
        z0 = b[1] * Xi + z1 - a1 * Yi;
        z1 = b[2] * Xi      - a2 * Yi;
        Y[i++] = Yi;
     }
     *Z++ = z0;
     *Z++ = z1;
  }

  return;
}

// =============================================================================
void CoreDouble4(double *X, int MX, int NX, double *a, double *b,
                 double *Z, double *Y)
{
  double Xi, Yi, z0, z1, z2, a1 = a[1], a2 = a[2], a3 = a[3];
  int i = 0, C;
  
  while (NX--) {
     z0 = Z[0];
     z1 = Z[1];
     z2 = Z[2];
     C  = i + MX;
     while (i < C) {
        Xi = X[i];
        Yi = b[0] * Xi + z0;
        z0 = b[1] * Xi + z1 - a1 * Yi;
        z1 = b[2] * Xi + z2 - a2 * Yi;
        z2 = b[3] * Xi      - a3 * Yi;
        Y[i++] = Yi;
     }
     *Z++ = z0;
     *Z++ = z1;
     *Z++ = z2;
  }

  return;
}

// =============================================================================
void CoreDouble5(double *X, int MX, int NX, double *a, double *b,
                 double *Z, double *Y)
{
  double Xi, Yi, z0, z1, z2, z3, a1 = a[1], a2 = a[2], a3 = a[3], a4 = a[4];
  int i = 0, C;
  
  while (NX--) {
     z0 = Z[0];
     z1 = Z[1];
     z2 = Z[2];
     z3 = Z[3];
     C  = i + MX;
     while (i < C) {
        Xi = X[i];
        Yi = b[0] * Xi + z0;
        z0 = b[1] * Xi + z1 - a1 * Yi;
        z1 = b[2] * Xi + z2 - a2 * Yi;
        z2 = b[3] * Xi + z3 - a3 * Yi;
        z3 = b[4] * Xi      - a4 * Yi;
        Y[i++] = Yi;
     }
     *Z++ = z0;
     *Z++ = z1;
     *Z++ = z2;
     *Z++ = z3;
  }

  return;
}

// =============================================================================
void CoreDouble6(double *X, int MX, int NX, double *a, double *b,
                 double *Z, double *Y)
{
  double Xi, Yi, z0, z1, z2, z3, z4,
         a1 = a[1], a2 = a[2], a3 = a[3], a4 = a[4], a5 = a[5];
  int i = 0, C;
  
  while (NX--) {
     z0 = Z[0];
     z1 = Z[1];
     z2 = Z[2];
     z3 = Z[3];
     z4 = Z[4];
     C  = i + MX;
     while (i < C) {
        Xi = X[i];
        Yi = b[0] * Xi + z0;
        z0 = b[1] * Xi + z1 - a1 * Yi;
        z1 = b[2] * Xi + z2 - a2 * Yi;
        z2 = b[3] * Xi + z3 - a3 * Yi;
        z3 = b[4] * Xi + z4 - a4 * Yi;
        z4 = b[5] * Xi      - a5 * Yi;
        Y[i++] = Yi;
     }
     *Z++ = z0;
     *Z++ = z1;
     *Z++ = z2;
     *Z++ = z3;
     *Z++ = z4;
  }

  return;
}

// =============================================================================
void CoreDouble7(double *X, int MX, int NX, double *a, double *b,
                 double *Z, double *Y)
{
  // Still 33% faster than the loop method.
  double Xi, Yi, z0, z1, z2, z3, z4, z5,
         a1 = a[1], a2 = a[2], a3 = a[3], a4 = a[4], a5 = a[5], a6 = a[6];
  int i = 0, C;
  
  while (NX--) {
     z0 = Z[0];
     z1 = Z[1];
     z2 = Z[2];
     z3 = Z[3];
     z4 = Z[4];
     z5 = Z[5];
     C  = i + MX;
     while (i < C) {
        Xi = X[i];
        Yi = b[0] * Xi + z0;
        z0 = b[1] * Xi + z1 - a1 * Yi;
        z1 = b[2] * Xi + z2 - a2 * Yi;
        z2 = b[3] * Xi + z3 - a3 * Yi;
        z3 = b[4] * Xi + z4 - a4 * Yi;
        z4 = b[5] * Xi + z5 - a5 * Yi;
        z5 = b[6] * Xi      - a6 * Yi;
        Y[i++] = Yi;
     }
     *Z++ = z0;
     *Z++ = z1;
     *Z++ = z2;
     *Z++ = z3;
     *Z++ = z4;
     *Z++ = z5;
  }
  
  return;
}

// *****************************************************************************
// ***                             DOUBLE REVERSE                            ***
// *****************************************************************************

void CoreDoubleNR(double *X, int MX, int NX, double *a, double *b,
                  int order, double *Z, double *Y)
{
  // Method for general filter length.
  // Signal X is process backwards, but a, b, and  Z have standard direction.
   
  double Xi, Yi;
  int i, j, R;
  
  R = 0;
  while (NX--) {
     i = R + MX - 1;
     while (i >= R) {
        Xi = X[i];
        Yi = b[0] * Xi + Z[0];
        for (j = 1; j < order; j++) {
           Z[j - 1] = b[j] * Xi + Z[j] - a[j] * Yi;
        }
        Z[order - 1] = b[order] * Xi - a[order] * Yi;
        
        Y[i--] = Yi;
     }
     Z += order;
     R += MX;
  }
  
  return;
}


