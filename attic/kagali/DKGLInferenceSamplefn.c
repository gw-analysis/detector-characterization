
#include <kagali/KGLStdlib.h>
#include <KGLInferenceSamplefn.h>
#include <DKGLInferenceSamplefn.h>

void DKGLInferenceSamplefn(double *in,int nin,double *out,int nout){
  KGLStatus *status = KGLCreateStatus();  
  
  double outtmp;
  for(int j = 0; j < nout; j++){
    out[j] = 0;
    for(int i = 0; i < nin; i++){
      outtmp = KGLInferenceSamplefn(status, in[i]);
      out[j] += (i+j)*outtmp;
    }
  }
  
  KGLDestroyStatus(status);
  
  return;
}
