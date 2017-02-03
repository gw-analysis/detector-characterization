/**
 * \author Koh Ueno 
 * \file
 *
 * \brief
 * An example showing how to find the BP test statistic.
 *
 * ### Prototypes ###
 *
 *
 * ### Description ###
 * Steps:
 * 1) Generate a chirping signal
 * 2) Do a chirplet transform
 * 3) Fill in chirplet costs in network
 * 4) Calculate the BP for prescribed path lengths test statistic
 * 5) Plot chirplet paths corresponding to the lengths
 *   chosen for the BP test statistic.
 *
 * ### Algorithm ###
 *
 *
 * ### Uses ###
 *
 * \code
 *
 * \endcode
 *
 */

#include <kagali/KGLStdlib.h>
#include <kagali/KGLTimeStamp.h>
#include <kagali/RealFFT.h>
#include <complex.h>
#include <kagali/KGLNoisePSD.h>
#include <kagali/KGLLinearAlgebra.h>


#define KGLCalloc4TensorAbortIfError(t,nn1,nn2,nn3,nn4,type,status) do { \
    size_t n1 = nn1; size_t n2 = nn2; size_t n3 = nn3; size_t n4 = nn4;	\
    t = (type ****) malloc(n1*sizeof(type***));				\
    for (int i=0; i<n1; i++) {						\
      t[i] = (type ***) malloc(n2*sizeof(type**));			\
      if(t[i]==NULL){							\
	KGLAddError(status,"fail to alloc 1 \"%s\"",#t);		\
	KGLAbort(status);						\
      }									\
      for (int j=0; j<n2; j++) {					\
	t[i][j] = (type **) malloc(n3*sizeof(type*));			\
	if (t[i][j] == NULL){						\
	  KGLAddError(status,"fail to alloc 2 \"%s\"",#t);		\
	  KGLAbort(status);						\
	}								\
	for (int k=0; k<n3; k++) {					\
	  t[i][j][k] = (type *) calloc(n4,sizeof(type));		\
	  if (t[i][j][k] == NULL){					\
	    KGLAddError(status,"fail to alloc 3 \"%s\"",#t);		\
	    KGLAbort(status);						\
	  }								\
	}								\
      }									\
    }									\
  } while(0)								\
    

void KGLNormalizeComplexVector( //begin{proto}
     KGLStatus *status, /**< status */
     double complex *x, /**< [out] */
     int N
     );

void KGLNormalizeComplexVector( //begin{proto}
    KGLStatus *status, /**< status */
    double complex *x, /**< [out] */
    int N
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  double xnorm = 0;
  for(int i = 0; i < N; i++){
    xnorm += x[i]*conj(x[i]);
  }
  for(int i = 0; i < N; i++){  // normalize so the signal has l2-norm 1
    x[i] /= sqrt(xnorm);
  }
  
  return;
}


void KGLGetSlopes( //begin{proto}
    KGLStatus *status, /**< status */
    double **slopes, /**< [out] */
    double *parameters1,
    double *parameters2,
    int *parameters3
    );

void KGLGetSlopes( //begin{proto}
    KGLStatus *status, /**< status */
    double **slopes, /**< [out] */
    double *parameters1,
    double *parameters2,
    int *parameters3
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int nScales = parameters3[1]-parameters3[0];
  
  for (int k = 0; k <= nScales; k++){
    int count = 0;
    // same slope range for every scale
    for (double d = parameters1[0]; d<=parameters1[1]; d += parameters2[k]){
      slopes[k][count] = d;
      count ++;
    }
  }
  
  return;
}

void KGLGetChirpletGraphParam( //begin{proto}
    KGLStatus *status, /**< status */
    int *parameters1, /**< [out] */
    int *parameters2, /**< [out] */
    double **parameters3, /**< [out] */
    double *parameters4, /**< [out] */
    int *parameters5, /**< [out] */
    char *parameters6, /**< [out] */
    int N,
    int csc,
    int fsc,
    double sldf,
    double *slopeRange,
    int minfreq,
    int maxfreq,
    char *xttype,
    int *degrees
    );

void KGLGetChirpletGraphParam( //begin{proto}
    KGLStatus *status,    /**< status */
    int *parameters1,     /**< [out] */
    int *parameters2,     /**< [out] */
    double **parameters3, /**< [out] */
    double *parameters4,  /**< [out] */
    int *parameters5,     /**< [out] */
    char *parameters6,    /**< [out] */
    int N,
    int csc,
    int fsc,
    double sldf,
    double *slopeRange,
    int minfreq,
    int maxfreq,
    char *xttype,
    int *degrees
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int J = ceil(log2(N)); // dyadic length of signal
  if (pow(2,J) != N){
    printf("J = %d; N = %d\n",J,N);
    printf("GetChirpletGraphParam: Signal length is not dyadic. Should be of the form N=2^J.");
  }
  if (csc > fsc){
    printf("ERROR: The parameters csc and fsc must satisfy csc<=fsc.");
  }
  if ((minfreq > maxfreq) | (minfreq < 0) | (maxfreq >= N)){
    printf("minfreq = %d maxfreq = %d N = %d\n",minfreq,maxfreq,N);
    printf("ERROR: The parameters minfreq and maxfreq must satisfy 0<= minfreq <= maxfreq <= N-1.");
  }
  parameters1[0] = N;
  parameters1[1] = J;
  parameters2[0] = csc;
  parameters2[1] = fsc;
  
  double *slopeparam1 = NULL;
  double *slopeparam2 = NULL;
  int *slopeparam3 = NULL;
  KGLCalloc(slopeparam1,2,double,status);
  KGLCalloc(slopeparam2,fsc-csc+1,double,status);
  KGLCalloc(slopeparam3,2,int,status);
  slopeparam1[0] = slopeRange[0];
  slopeparam1[1] = slopeRange[1];
  double slopeDifference = fabs(slopeRange[1]-slopeRange[0]);
  
  for (int s = csc; s <= fsc; s++){
    int nx = pow(2,(J-s));
    if (sldf/nx > 1){
      // just to make sure that we have at least 2 slopes per scale
      // in the case of the finest allowable scale (s=J-1)
      slopeparam2[s-csc] = slopeDifference/2;
    }else{
      slopeparam2[s-csc] = (slopeDifference/2)/nx*sldf;
    }
  }
  slopeparam3[0] = parameters2[0];
  slopeparam3[1] = parameters2[1];
    
  KGLGetSlopes(status,parameters3,slopeparam1,slopeparam2,slopeparam3);
    
  // set maximum and minimum frequencies
  parameters4[0] = minfreq;
  parameters4[1] = maxfreq;
  
  // set the type of chirplet transform to use
  strcpy(parameters6, xttype);
  
  free(slopeparam1);
  free(slopeparam2);
  free(slopeparam3);
  
  if(strcmp(xttype,"VARAMP")){
    for(int i = csc; i <= fsc; i++){
      degrees[i] = 2;
    }
  }
  
  // set the degrees for the polynomial fit of the amplitude
  for(int i = csc; i <= fsc; i++){
    parameters5[i] = degrees[i];
  }
  
  return;
}


void KGLCTPlain( //begin{proto}
    KGLStatus *status,   /**< status */
    double complex ****cc, /**< [out] */ 
    double complex *data,
    int *parameters1,
    int *parameters2,
    double **parameters3,
    double *parameters4,
    int *parameters5,
    char *parameters6
    );

void KGLCTPlain( //begin{proto}
    KGLStatus *status,     /**< status */
    double complex ****cc, /**< [out] */ 
    double complex *data,
    int *parameters1,
    int *parameters2,
    double **parameters3,
    double *parameters4,
    int *parameters5,
    char *parameters6
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int N = parameters1[0];
  int J = parameters1[1];
  int csc = parameters2[0];
  int fsc = parameters2[1];
  double *slopes = NULL;
  double *t = NULL;
  int *ix = NULL;
  double complex **chirppart = NULL;
  double complex **X = NULL;
  double complex **XF = NULL;
  KGLCalloc(slopes,N+1,double,status);
  KGLCalloc(t,N,double,status);
  KGLCalloc(ix,N,int,status);
  KGLCallocMatrix(chirppart,N+1,N,double complex,status);
  KGLCallocMatrix(XF,N+1,N,double complex,status);
  
  for (int s = fsc; s >= csc; s--){ // loop for scale
    for (int i = 0; i <= (int)pow(2,J-s); i++){
        slopes[i] = parameters3[s-csc][i];
      }
    for (int j = 0; j < (int)pow(2,J-s); j++){
      t[j] = (double)j;
    }
    for (int i = 0; i <= (int)pow(2,J-s); i++){ // loop for slope
      for (int j = 0; j < (int)pow(2,J-s); j++){ // loop for time
	chirppart[i][j] = cexp(-I * KGL_PI * slopes[i] * pow(t[j],2)/N);
      }
    }
    
    for (int b = 0; b < (int)pow(2,s); b++){
      int nblocks = (int)pow(2,s);
      int count = 0;
      for(int ii = b*N/nblocks + 1; ii <= (b+1)*N/nblocks; ii++){
	ix[count] = ii;
	count++;
      }
      int lenix = count;

      KGLCallocMatrix(X,N+1,N,double complex,status);
      for (int i = 0; i <= (int)pow(2,J-s); i++){ // loop for slope
	for (int j = 0; j < (int)pow(2,J-s); j++){ // loop for time
	  X[i][j] = data[ix[j]-1] * chirppart[i][j];
	}
      }
      
      fftw_complex *in, *out;
      fftw_plan p;
      in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
      out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
      for (int i = 0; i <= (int)pow(2,J-s); i++){ // loop for slope
	for(int j = 0; j < N; j++) in[j] = X[i][j]; // loop for time
	p = fftw_plan_dft_1d(N, in, out, FFTW_FORWARD, FFTW_ESTIMATE);
	fftw_execute(p); /* repeat as needed */
	fftw_destroy_plan(p);
	for(int j = 0; j < N; j++){
	  XF[i][j] = out[j] / sqrt(lenix);
	}
      }
      fftw_free(in); fftw_free(out);      
      
      for (int i = 0; i <= (int)pow(2,J-s); i++){ // loop for slope
	for(int j = 0; j < N; j++){ // loop for frequency offset
	  cc[s][b][i][j] = XF[i][j];
	}
      }
      free(X);
    }
  }
  
  free(slopes);
  free(t);
  free(ix);
  free(chirppart);
  free(XF);
  
  return;
}


void KGLChirpletTransform( //begin{proto}
    KGLStatus *status,   /**< status */
    double complex ****cc, /**< [out] */ 
    double complex *data,
    int *param1,
    int *param2,
    double **param3,
    double *param4,
    int *param5,
    char *param6
    );

void KGLChirpletTransform( //begin{proto}
    KGLStatus *status,     /**< status */
    double complex ****cc, /**< [out] */ 
    double complex *data,
    int *param1,
    int *param2,
    double **param3,
    double *param4,
    int *param5,
    char *param6
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
    
  if(strcmp(param6,"PLAIN") == 0){
    KGLCTPlain(status,cc,data,param1,param2,param3,
	       param4,param5,param6);
  }
  
  return;
}


void KGLGetChirpletNetwork( //begin{proto}
    KGLStatus *status,  /**< status */
    int **icnetwork1,   /**< [out] */ 
    double **cnetwork1, /**< [out] */ 
    int *cnetwork2,     /**< [out] */ 
    int *cnetwork3,     /**< [out] */ 
    int *cnetwork4,     /**< [out] */ 
    int *cnetwork5,     /**< [out] */ 
    double complex ****cc,
    int *parameters1,
    int *parameters2,
    double **parameters3,
    double *parameters4,
    int *parameters5,
    char *parameters6,
    int coarsestScale
    );

void KGLGetChirpletNetwork( //begin{proto}
    KGLStatus *status,  /**< status */
    int **icnetwork1,   /**< [out] */ 
    double **cnetwork1, /**< [out] */ 
    int *cnetwork2,     /**< [out] */ 
    int *cnetwork3,     /**< [out] */ 
    int *cnetwork4,     /**< [out] */ 
    int *cnetwork5,     /**< [out] */ 
    double complex ****cc,
    int *parameters1,
    int *parameters2,
    double **parameters3,
    double *parameters4,
    int *parameters5,
    char *parameters6,
    int coarsestScale
  ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int N = parameters1[0];
  int J = parameters1[1];
  int fsc = parameters2[1];  
  //printf("fsc = %d\n",fsc);
  
  double frmin = parameters4[0];
  double frmax = parameters4[1];
  int nfreqs = frmax-frmin+1;
  int ntfnodes;
  int *startfreqs = NULL;
  KGLCalloc(startfreqs,N+1,int,status);
  
  int count = 0;
  for(int d = frmin; d <= frmax; d++){
    startfreqs[count] = d;
    count ++;
  }
  ntfnodes = nfreqs * (int)pow(2,fsc);
  
  int iapp = 0;
  int leftindexmax = 0;
  int rightindexmax = 0;
  
  for (int s = fsc; s >= coarsestScale; s--){
    // Calculate the changes in frequency. 
    // A time interval at scale s is of length 2^(J-s)
    int *dfreq = NULL;
    int **endfreqs = NULL;
    int **iendfreqs = NULL;
    KGLCalloc(dfreq,N+1,int,status);
    KGLCallocMatrix(endfreqs,N,N+1,int,status);
    KGLCallocMatrix(iendfreqs,N,N+1,int,status);
    
    for(int i = 0; i <= (int)pow(2,J-s); i++){
      dfreq[i] = parameters3[s-parameters2[0]][i] * (int)pow(2,(J-s)); 
    }
    for(int i = 0; i < N; i++){
      for(int j = 0; j <= (int)pow(2,J-s); j++){
	endfreqs[i][j] = startfreqs[i]+dfreq[j];
	// get the frequency as an integer
	if(endfreqs[i][j] >= 0){
	  iendfreqs[i][j] = (int)endfreqs[i][j] % N;
	}else{
	  iendfreqs[i][j] = (int)endfreqs[i][j] + N;
	}
      }
    }
    
    int **slopeindices = NULL;
    KGLCallocMatrix(slopeindices,N,N+1,int,status);
    
    if ((int)frmax == N-1){
      // the frequencies have period N, 
      // want a value between 0 and N-1.
      for (int m = 0; m < nfreqs; m++){
	// allow all slope indices
	for (int i = 0; i <= (int)pow(2,J-s); i++){
	  slopeindices[m][i] = i;
	}
      }
    }else{
    // we will only use chirplets with end-frequency between the allowed range   
    }
    
    // loop over time indices and find the connecting chirplets
    for (int b = 0; b <= (int)pow(2,s)-1; b++){
      int leftoffset = b * pow(2,fsc-s)*nfreqs + 1-frmin;
      int rightoffset = (b+1) * pow(2,fsc-s)*nfreqs + 1-frmin;
      
      int *leftindex = NULL;
      int **rightindex = NULL;
      KGLCalloc(leftindex,N+1,int,status);
      KGLCallocMatrix(rightindex,N,N+1,int,status);
      for(int i = 0; i < N; i++){
	leftindex[i] = startfreqs[i] + leftoffset;
	if(leftindexmax < leftindex[i]) leftindexmax = leftindex[i];
      }
      for(int i = 0; i < N; i++){
	for(int j = 0; j <= (int)pow(2,J-s); j++){
	  rightindex[i][j] = iendfreqs[i][j] + rightoffset;
	  if(rightindexmax < rightindex[i][j]) rightindexmax = rightindex[i][j];
	}
      }
     
      for (int m = 0; m < nfreqs; m++){
	for(int i = 0; i <= (int)pow(2,J-s); i++){
	  icnetwork1[leftindex[m]][iapp+i] = rightindex[m][slopeindices[m][i]];
	  double complex ctmp = cc[s][b][slopeindices[m][i]][(int)startfreqs[m]];
	  double modulus2 = ctmp*conj(ctmp);
	  cnetwork1[leftindex[m]][iapp+i] = -modulus2;
	}
      }
      free(leftindex);
      free(rightindex);
    }
    free(dfreq);
    free(endfreqs);
    free(iendfreqs);
    free(slopeindices);
    iapp += (int)pow(2,J-s)+1;
  }
    
  // list the start nodes and end nodes
  *cnetwork2 = ntfnodes + nfreqs; // number of nodes in the graph

  count = 0;
  for(int i = 1; i <= nfreqs; i++){
    cnetwork3[count] = i; // the topological ordering of startnodes
    cnetwork4[count] = ntfnodes + i; // the topological ordering of endnodes
    count++;
  }
  count = 0;
  for(int i = 1; i <= ntfnodes+nfreqs; i++){
    cnetwork5[count] = i;  // The topological ordering (the nodes where numbered that way)
    count++;
  }
  
  free(startfreqs);

  return;
}


void KGLShortestPathCell( //begin{proto}
    KGLStatus *status,  /**< status */
    double *thecost,    /**< [out] */
    double *costs,      /**< [out] */
    int **paths,        /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    double alpha,
    int N,
    int nfreqs
    );

void KGLShortestPathCell( //begin{proto}
    KGLStatus *status,  /**< status */
    double *thecost,    /**< [out] */
    double *costs,      /**< [out] */
    int **paths,        /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    double alpha,
    int N,
    int nfreqs
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int nG = N*N/2;
  int nLeftEndpoints = nG;  
  int nNodes = cnetwork2;

  // Assume that the nodes in G appear in topological order.
  int *ord = NULL;
  KGLCalloc(ord,nG,int,status);
  for(int i = 0; i < nG; i++){
    ord[i] = cnetwork5[i];
  }
  int *startNodes = NULL;
  KGLCalloc(startNodes,nfreqs,int,status);
  int count = 0;
  for(int i = 0; i < nfreqs; i++){
    if(cnetwork3[i] == 0) break;
    startNodes[i] = cnetwork3[i];
    count ++;
  }
  int nStartNodes = count;

  // Initialize the distance label matrix
  int iinf = 999;
  double *d = NULL;
  int *pred = NULL;
  KGLCalloc(d,nNodes,double,status);
  KGLCalloc(pred,nNodes,int,status);
  for(int i = 0; i < nNodes; i++){
    d[i] = iinf;
  }
  // all the startnodes have distance label 0
  for(int i = 0; i < nStartNodes; i++) {
    d[startNodes[i]-1] = 0;
  }
  
  int nArcs = 0;
  int arcIndex;
  double tentativeDistance = 0;
  
  // Examine nodes in topological order
  for(int startNode = 1; startNode <= nLeftEndpoints; startNode++) {
    
    // arcs is the set of endpoints of arcs emanating from startNode
    count = 0;
    for (int k = 0; k < N*N; k++) {
      if(icnetwork1[startNode][k] != 0){
	count ++;
      }else{
	break;
      }
    }
    nArcs = count;
    
    for (int k = 0; k < nArcs; k++) {
      // calculate a tentative distance label, 
      // updating cost by subtracting alpha 
      tentativeDistance = d[ord[startNode-1]-1]
	+ cnetwork1[startNode][k] - alpha;
      arcIndex = icnetwork1[startNode][k] - 1;
      if( d[arcIndex] > tentativeDistance ) {
        d[arcIndex] = tentativeDistance;
	pred[arcIndex] = ord[startNode-1] - 1;
	costs[arcIndex] = d[arcIndex];
      }
    }
  }
  
  int ind = 0;
  double dmin = iinf;
  for(int i = 0; i < nfreqs; i++){
    if(dmin > d[cnetwork4[i]]){
      dmin = d[cnetwork4[i]];
      ind = i;
    }
  }
  *thecost = dmin;

  count = 0;
  paths[0][0] = cnetwork4[ind];
  for(int i = 0; i <= nNodes; i++){
    paths[0][i+1] = pred[paths[0][i]];
    if(paths[0][i] == 0) break;
    count++;
  }
  
  int *pathtmp = NULL;
  KGLCalloc(pathtmp,count,int,status);
  for(int i = 0; i < count; i++){
    pathtmp[i] = paths[0][i] + 1;
  }
  for(int i = 0; i < count; i++){
    paths[0][i] = pathtmp[count-i-1]; // make it in increasing order
  }

  free(startNodes);
  free(pathtmp);
  free(d);
  free(pred);
  
  return;
}

void KGLCSPCell( //begin{proto}
    KGLStatus *status, /**< status */
    double *cost,  /**< [out] */
    int **paths, /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    int maxLength,
    int N,
    int nfreqs
    );

void KGLCSPCell( //begin{proto}
    KGLStatus *status, /**< status */
    double *cost,  /**< [out] */
    int **paths, /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    int maxLength,
    int N,
    int nfreqs
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int nG = N*N/2;
  
  // Assume that the nodes in G appear in topological order.
  int *ord = NULL;
  KGLCalloc(ord,nG,int,status);
  for(int i = 0; i < nG; i++){
    ord[i] = i+1;
  }
  
  // Initialize the distance label matrix
  int iinf = 999;
  double **d = NULL;
  KGLCallocMatrix(d,maxLength+1,cnetwork2,double,status);
  for(int i = 0; i <= maxLength; i++){
    for(int j = 0; j < cnetwork2; j++){
      d[i][j] = iinf;
    }
  }
  
  // all the startnodes have distance label 0
  for(int j = 0; j < nfreqs; j++) d[0][j] = 0;
  
  int **pred = NULL;
  KGLCallocMatrix(pred,maxLength+1,cnetwork2,int,status);
  
  // if we are only interested in costs, 
  // we do not need to keep predessor indices. 
  // Can save a bit of memory by considering these two cases
  // Initialize the predecessor indices
  for(int j = 0; j < nfreqs; j++) pred[0][j] = j+1;
  
  // Examine nodes in topological order
  int lenA = 0;
  int *A = NULL;
  double *L = NULL;
  KGLCalloc(A,nG,int,status);
  KGLCalloc(L,nG,double,status);
  
  for (int start = 1; start <= nG; start++){
    for (int i = 0; i < N*N; i++){
      if(cnetwork1[start][i] != 0){
	A[i] = icnetwork1[start][i]; // A is the set of endpoints and costs of arcs emanating out of node 'start'.
	L[i] = cnetwork1[start][i]; // costs
      }else{
	lenA = i;
	break;
      }
    }
    for (int k = 0; k < lenA; k++){
      for (int l = 1; l <= maxLength; l++){
	if ((int)d[l-1][start-1] != iinf){
	  // PEND: Could check for Inf values earlier and then
	  // loop over the resulting set.
	  
	  if ( d[l][A[k]-1] > d[l-1][start-1] + L[k]){
	    d[l][A[k]-1] = d[l-1][start-1] + L[k];
	    pred[l][A[k]-1] = start;
	  }
	}
      }
    }
  }
  
  // Now we figure out which of the endnodes has the lowest distance label. 
  // That is going to be the end node of our shortest path. 
  // To find the shortest path we just have to start at
  // the endnode and track the predecessor nodes.
  int *ind = NULL;
  double *costtmp = NULL;
  KGLCalloc(ind,maxLength+1,int,status);
  KGLCalloc(costtmp,maxLength+1,double,status);
  
  for(int i = 0; i <= maxLength; i++){
    costtmp[i] = iinf;
  }
    
  double dmin;
  for (int m = 0; m <= maxLength; m++){
    dmin = iinf;
    for (int i = 0; i < nfreqs; i++){
      if(dmin > d[m][cnetwork4[i]-1]){
	dmin = d[m][cnetwork4[i]-1];
	costtmp[m] = dmin;
	ind[m] = i;
      }
    }
  }
  
  for (int m = 0; m < maxLength; m++){
    cost[m] = costtmp[m+1];
  }
    
  // the caller wanted to get the paths, so find them 
  for (int k = 0; k < maxLength; k++){
    paths[k][k+1] = cnetwork4[ind[k+1]];
    for (int l = k; l >= 0; l--){
      paths[k][l] = pred[l+1][paths[k][l+1]-1];
    }
  }
  
  return;
}

void KGLMinCTRatioCells( //begin{proto}
    KGLStatus *status, /**< status */
    double *cost,  /**< [out] */
    int **paths, /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    double alphamax,
    int N,
    int nfreqs
    );

void KGLMinCTRatioCells( //begin{proto}
    KGLStatus *status, /**< status */
    double *cost,  /**< [out] */
    int **paths, /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    double alphamax,
    int N,
    int nfreqs
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  double *Scost = NULL;
  KGLCalloc(Scost,N*N,double,status);
  double thecost;
  KGLShortestPathCell(status,&thecost,Scost,paths,icnetwork1,cnetwork1,
		      cnetwork2,cnetwork3,cnetwork4,cnetwork5,
		      alphamax,N,nfreqs);
  
  // 'cost' is the cost of this path
  // 'paths' is the list of nodes on path
  int lpathsum = 0;
  double *alpha = NULL;
  int *lpaths = NULL;
  int *pp = NULL;
  KGLCalloc(alpha,N,double,status);
  KGLCalloc(lpaths,N,int,status);
  KGLCalloc(pp,N*N,int,status);
  alpha[0] = alphamax;
  
  int count = 0;
  for(int i = 0; i < N; i++){
    if(paths[0][i] != 0){
      pp[i] = paths[0][i];
      count ++;
    }else{
      break;
    }
  }
  
  int lpath = count - 1;
  lpaths[0] = lpath; 
  lpathsum = lpath + 1;
  int count2 = 1;
  while (thecost < 0 && fabs(thecost) > 1e-3){
    // we have not found the optimum value, 
    // but we have a better upper bound 
    alphamax = thecost/lpath + alphamax;
    KGLShortestPathCell(status,&thecost,Scost,paths,icnetwork1,cnetwork1,
			cnetwork2,cnetwork3,cnetwork4,cnetwork5,
			alphamax,N,nfreqs);
    count = 0;
    for(int i = 0; i < N; i++){
      if(paths[0][i] != 0){
	pp[i+lpathsum] = paths[0][i];
	count ++;
      }else{
	break;
      }
    }
    lpath = count - 1;
    lpathsum += lpath + 1 ;
        
    alpha[count2] = alphamax;
    lpaths[count2] = lpath;
    count2++;
  }
  // When we exit the loop, the modified cost is ~ 0 and we hold 
  // the optimal value
  
  count = 1;
  for(int i = 1; i < N; i++){
    if(alpha[i] != 0){
      *cost = alpha[i];
      count ++;
    }else{
      break;
    }
  }
  int npaths = count;
  printf("npaths = %d\n",npaths);
    
  free(alpha);
  free(lpaths);
  free(pp);

  return;
}


void KGLCalculateStatistic( //begin{proto}
    KGLStatus *status,  /**< status */
    double *cost,       /**< [out] */
    int **paths,        /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    char *STATTYPE, 
    double alpha,
    int N,
    int nfreqs
    );

void KGLCalculateStatistic( //begin{proto}
    KGLStatus *status,  /**< status */
    double *cost,       /**< [out] */
    int **paths,        /**< [out] */
    int **icnetwork1,   /**< [in] */ 
    double **cnetwork1, /**< [in] */ 
    int cnetwork2,      /**< [in] */ 
    int *cnetwork3,     /**< [in] */ 
    int *cnetwork4,     /**< [in] */ 
    int *cnetwork5,     /**< [in] */ 
    char *STATTYPE, 
    double alpha,
    int N,
    int nfreqs
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  if(strcmp(STATTYPE,"SP") == 0){
    double thecost;
    KGLShortestPathCell(status,&thecost,cost,paths,icnetwork1,cnetwork1,
			cnetwork2,cnetwork3,cnetwork4,cnetwork5,
			alpha,N,nfreqs);
  }
  
  if(strcmp(STATTYPE,"BPFORPLOTTING") == 0){
    int maxLength = (int)alpha;
    KGLCSPCell(status,cost,paths,
	       icnetwork1,cnetwork1,cnetwork2,cnetwork3,
	       cnetwork4,cnetwork5,maxLength,N,nfreqs);
  }
  
  if(strcmp(STATTYPE,"MCTR") == 0){
    double costtmp;
    KGLMinCTRatioCells(status,&costtmp,paths,
		       icnetwork1,cnetwork1,cnetwork2,cnetwork3,
		       cnetwork4,cnetwork5,alpha,N,nfreqs);
    cost[0] = costtmp;
  }
  
  return;
}


void KGLChirpletAnalysis( //begin{proto}
    KGLStatus *status,    /**< status */
    int **paths,          /**< [out] */
    double *costpath,     /**< [out] */
    double complex *data, /**< [in] */
    int N,
    int fsc,
    int csc,
    int maxfreq,
    int minfreq,
    double alpha,
    char *XTTYPE,
    char *STATTYPE,
    char *COMPLEX_OR_REAL
    );

void KGLChirpletAnalysis( //begin{proto}
    KGLStatus *status,    /**< status */
    int **paths,          /**< [out] */
    double *costpath,     /**< [out] */
    double complex *data, /**< [in] */
    int N,
    int fsc,
    int csc,
    int maxfreq,
    int minfreq,
    double alpha,
    char *XTTYPE,
    char *STATTYPE,
    char *COMPLEX_OR_REAL
    )//end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int nfreqs = maxfreq-minfreq+1;
  
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  // CHIRPLET TRANSFORM
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  //  printf("Taking chirplet transform...\n");
  
  int J = ceil(log2(N));
  int *graphparam1 = NULL;
  int *graphparam2 = NULL;
  double **graphparam3 = NULL;
  double *graphparam4 = NULL;
  int *graphparam5 = NULL;
  char *graphparam6 = NULL;  
  graphparam6 = malloc(strlen(XTTYPE));
  
  KGLCalloc(graphparam1,2,int,status);
  KGLCalloc(graphparam2,2,int,status);
  KGLCallocMatrix(graphparam3,J,(int)pow(2,J)+1,double,status);
  KGLCalloc(graphparam4,2,double,status);
  KGLCalloc(graphparam5,J,int,status);
  
  double sldf = 2;
  double *slopeRange = NULL;
  KGLCalloc(slopeRange,2,double,status);
  slopeRange[0] = -0.5;
  slopeRange[1] = 0.5;
  
  int *degrees = NULL;
  KGLCalloc(degrees,J,int,status);
  for (int i = 0; i <= fsc-csc; i++) degrees[i] = 2;
  
  KGLGetChirpletGraphParam(status,graphparam1,graphparam2,graphparam3,
                           graphparam4,graphparam5,graphparam6,
			   N,csc,fsc,sldf,slopeRange,minfreq,maxfreq,
			   XTTYPE,degrees);
  
  double complex ****cc = NULL;
  KGLCalloc4TensorAbortIfError(cc,J,N,N+1,N,double complex,status);
  
  KGLChirpletTransform(status,cc,data,graphparam1,graphparam2,graphparam3,
                       graphparam4,graphparam5,graphparam6);
  
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  // SET UP CHIRPLET NETWORK AND FILL IN CHIRPLET COSTS
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  // printf("Generating chirplet graph and assigning costs... (this will take a while)\n");
  
  int **icnetwork1 = NULL;
  double **cnetwork1 = NULL;
  int cnetwork2;
  int *cnetwork3 = NULL;
  int *cnetwork4 = NULL;
  int *cnetwork5 = NULL;
  
  if(strcmp(STATTYPE,"MCTR") != 0){
    KGLCallocMatrix(icnetwork1,N*N,N*N,int,status);
    KGLCallocMatrix(cnetwork1,N*N,N*N,double,status);
    KGLCalloc(cnetwork3,maxfreq-minfreq+1,int,status);
    KGLCalloc(cnetwork4,maxfreq-minfreq+1,int,status);
    KGLCalloc(cnetwork5,N*(maxfreq-minfreq+1),int,status);
    
    int coarsestScale = graphparam2[0];
    KGLGetChirpletNetwork(status,icnetwork1,cnetwork1,&cnetwork2,cnetwork3,
			  cnetwork4,cnetwork5,cc,graphparam1,graphparam2,
			  graphparam3,graphparam4,graphparam5,graphparam6,
			  coarsestScale);
    
    //printf("Running optimization routine for graph...\n");  
        
    KGLCalculateStatistic(status,costpath,paths,
			  icnetwork1,cnetwork1,cnetwork2,cnetwork3,
			  cnetwork4,cnetwork5,STATTYPE,alpha,N,nfreqs);
    
    free(icnetwork1);
    free(cnetwork1);
    free(cnetwork3);
    free(cnetwork4);
    free(cnetwork5);
  }else{
    int count = 0;
    for(int coarsestScale = csc; coarsestScale < fsc; coarsestScale++){
      double *cost = NULL;
      KGLCalloc(cost,1,double,status);
      KGLCallocMatrix(icnetwork1,N*N,N*N,int,status);
      KGLCallocMatrix(cnetwork1,N*N,N*N,double,status);
      KGLCalloc(cnetwork3,maxfreq-minfreq+1,int,status);
      KGLCalloc(cnetwork4,maxfreq-minfreq+1,int,status);
      KGLCalloc(cnetwork5,N*(maxfreq-minfreq+1),int,status);
      
      printf("\n");
      printf("coarsestScale = %d\n",coarsestScale);
      KGLGetChirpletNetwork(status,icnetwork1,cnetwork1,&cnetwork2,cnetwork3,
			    cnetwork4,cnetwork5,cc,graphparam1,graphparam2,
			    graphparam3,graphparam4,graphparam5,graphparam6,
			    coarsestScale);
      
      printf("cnetwork2 = %d\n",cnetwork2);
      printf("Running optimization routine for graph...\n");  
      
      KGLCalculateStatistic(status,cost,paths,
			    icnetwork1,cnetwork1,cnetwork2,cnetwork3,
			    cnetwork4,cnetwork5,STATTYPE,alpha,N,nfreqs);
      costpath[count] = cost[0];
      printf("cost = %f\n",cost[0]);
      count ++;
      free(cost);
      free(icnetwork1);
      free(cnetwork1);
      free(cnetwork3);
      free(cnetwork4);
      free(cnetwork5);
    }
  }
  //printf("Done!\n");
  
  if(strcmp(COMPLEX_OR_REAL,"REAL") == 0 
     && strcmp(STATTYPE,"BPFORPLOTTING") == 0){
    for(int i = 0; i < (int)alpha; i++) costpath[i] *= 2.0;
  }
  
  free(cc);
  free(graphparam1);
  free(graphparam2);
  free(graphparam3);
  free(graphparam4);
  free(graphparam5);
  free(graphparam6);
  free(slopeRange);
  free(degrees);
  
  return;
}


void KGLChirpletMain( //begin{proto}
    KGLStatus *status,     /**< status */
    int **paths,           /**< [out] */
    double *cost,          /**< [out] */
    double complex *cframe, /**< [in] */
    char *COMPLEX_OR_REAL, /**< [in] */
    char *XTTYPE,
    char *STATTYPE,
    char *INJECTION,
    int fs,
    double alpha, /**< [in] Calculates BP for lengths 1,...,maxLength */
    int nframe
    );

void KGLChirpletMain( //begin{proto}
    KGLStatus *status,     /**< status */
    int **paths,           /**< [out] */
    double *cost,          /**< [out] */
    double complex *cframe, /**< [in] */
    char *COMPLEX_OR_REAL, /**< [in] */
    char *XTTYPE,
    char *STATTYPE,
    char *INJECTION,
    int fs,
    double alpha, /**< [in] Calculates BP for lengths 1,...,maxLength */
    int nframe
    ) //end{proto}
{
  if(status == NULL) {
    KGLPrintError("NULL status pointer passed");
    abort();
  }
  
  int jframe = ceil(log2(nframe)); // dyadic length of signal   
  int minfreq = 0; // restricting the chirplet graph to nonnegative frequencies
  int maxfreq = nframe-1;
  int csc = 0;
  int fsc = jframe-1;  
  // normalize so the signal has l2-norm 1
  KGLNormalizeComplexVector(status,cframe,nframe);    
  KGLChirpletAnalysis(status,paths,cost,cframe,
		      nframe,fsc,csc,maxfreq,minfreq,alpha,
		      XTTYPE,STATTYPE,COMPLEX_OR_REAL); 
  
  return;
}
