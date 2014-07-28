/************************************************/
/* File   : StatisticsUtils.h                   */
/* Author : Hirotaka Yuzurihara                 */
/* Time-stamp: "2014-07-28 16:56:34 yuzurihara" */
/************************************************/

#ifndef STATISTICSUTILS_H
#define STATISTICSUTILS_H

double calculatePeasonCorrelation(double *data1, double *data2, int dataLength);
double permutationTestPeasonCorrelation(double *data1, double *data2, int dataLength, int repeatTimesOfTest);

#endif
