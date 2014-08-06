/* compute percentile of observed correlation by permutation */
/* By Shuhei Mano 10/07/2014 */

/* modfied by Yuzurihara, 28/07/2014 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "./HasKAL/StatisticsUtils/StatisticsUtils.h"

#define MAX_DATA 100000

int main(int argc,char **argv){

  FILE *fp;
  int dataLength;
  int repeatTimesOfTest;
  int i;
  double pvalue;

  double data1[MAX_DATA];
  double data2[MAX_DATA];


  if(argc < 4){
    fprintf(stderr, "usage : $0 data1.txt data2.txt (int)repeatTimesOfTest\n");
  }else{

    if ((fp = fopen(argv[1], "r")) == NULL) {
      printf("file open error!!\n");
      exit(1);
    }
    i=0;
    while( fscanf(fp, "%le", &data1[i]) !=EOF){
      i++;
    }
    dataLength = i;
    
    if ((fp = fopen(argv[2], "r")) == NULL) {
      printf("file open error!!\n");
      exit(1);
    }
    i=0;
    while( fscanf(fp, "%le", &data2[i]) !=EOF){
      i++;
    }
    /* set smaller size as dataLength */
    if(i < dataLength) dataLength=i;

    repeatTimesOfTest = atoi(argv[3]);


    fprintf(stderr, "This is calculate in C\n");

    double peason = calculatePeasonCorrelation(data1, data2, dataLength);
    fprintf(stdout, "peason value = %lf\n", peason);

    pvalue = permutationTestPeasonCorrelation(data1, data2, dataLength, repeatTimesOfTest);
    fprintf(stdout, "Two sided tail prob.= %.10e\n", pvalue);


  } 
  return 0;
}




