void DKGLFIRBandPassFilter(double *dataout,double *datain,int npoint,double fs,double flow,double fhigh);
void DKGLButterworthBandPassFilter(double *dataout,double *datain,int npoint,double fs,double flow,double fhigh,int order);
void DKGLButterworthBandPassSOSFilter(double *dataout,double *datain,int npoint,double fs,double flow,double fhigh,int order);
void DKGLIterativeLeastSquare2DNewton(double *Afit,double *ffit,double *pfit,double *frame,double fs,int nframe,int nsig);
