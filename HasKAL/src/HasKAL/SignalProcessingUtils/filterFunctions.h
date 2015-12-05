
int fir_filter_core(double*,  unsigned,  double[],  double[],  unsigned*,  unsigned, double*);

int fir_filter (double*, unsigned, double[], unsigned,  double*);

int iir_filter (double*,  unsigned,  double[],  double[], unsigned, double*);

int iir_filter_core (double*, unsigned, double[], double[], unsigned, double[], double*);

double goertzel (double*, int, double, double);

int filtfilt (double*,  unsigned,  double[],  double[], unsigned, double*);

void sosfilter (double*,  unsigned,  double*,  double*,  double*,  double*,  double*,  double*,  unsigned,  double*);

double sosform1 (int,  double,  double*,  double*,  double*,  double*,  double*,  double*,  double*,  double*,  double*,  double*);

void sosstatespace (double*, unsigned, double*, double*, double*, double, double, double, double*);

