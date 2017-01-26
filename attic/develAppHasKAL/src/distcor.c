/* compute percentile of observed correlation by permutation */
/* based on distance correlation Szekely & Rizzo AAS 2009 */
/* By Shuhei Mano 12/21/2014 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_PTS 163840

distcor(double val1[],double val2[],int len,double* out) {

  int i,j;
  double var1,var2,cov;
  double am[MAX_PTS],bm[MAX_PTS];
  double amm,bmm;

  for(i=0;i<len;i++) {
    am[i]=bm[i]=0.0;
    for(j=0;j<len;j++) {
      am[i]+=fabs(val1[i]-val1[j])/len;
      bm[i]+=fabs(val2[i]-val2[j])/len;
    }
  }
  amm=bmm=0.0;
  for(i=0;i<len;i++) {
    amm+=am[i]/len;
    bmm+=bm[i]/len;
  }

  cov=0.0;
  for(i=0;i<len;i++) {
    for(j=0;j<len;j++) {
      cov+=(fabs(val1[i]-val1[j])-am[i]-am[j]+amm)
          *(fabs(val2[i]-val2[j])-bm[i]-bm[j]+bmm)/len/len;
    }
  }

  var1=0.0;
  var2=0.0;
  for(i=0;i<len;i++) {
    for(j=0;j<len;j++) {
    var1+=pow(fabs(val1[i]-val1[j])-am[i]-am[j]+amm,2.0)/len/len;
    var2+=pow(fabs(val2[i]-val2[j])-bm[i]-bm[j]+bmm,2.0)/len/len;
    }
  }

  *out = sqrt(cov/sqrt(var1*var2));
  return;
}
