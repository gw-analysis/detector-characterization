//
//
//
//
// const char* server = "ldas-pcdev1.ligo.caltech.edu";
//
// int           port = 31200;  
//
// const char* channel[4] = {
//     "H1:DMT-STRAIN", 
//     "L1:DMT-STRAIN", 
//     "V1:h16384Hz",
//     0
// }




#include "daqc.h"
#include "channel.h"
#include "daqc_response.h"
#include "nds.h"
#include "nds_logging.h"
#include "trench.h"
#include <stdio.h>


int nds_GetData (const char* server, int port, const char* channel[], int start_gps_in, int end_gps_in, int delta_in, float* data, int* length) {


//-- Initialize --
int rc = daq_startup();
if (rc) {
    printf("global initialization failed\n");
    return 1;
}

//-- Connect to server --
daq_t daqd;
rc = daq_connect(&daqd, server, port, nds_v2);
if (rc) {
    printf("Connection failed with error: %s\n", daq_strerror(rc));
    return 2;
}


int i;
for (i=0; i<4; i++) {
    rc = daq_request_channel(&daqd, channel+i, 0, 0);
}

//--  Request data --
time_t start_gps = start_gps_in;
time_t end_gps   = end_gps_in;
time_t delta     = delta_in;
rc = daq_request_data(&daqd, start_gps, end_gps, delta);
if (rc) {
    printf("Data request failed with error: %s\n", daq_strerror(rc));
    return 3;
}

//--  Read data blocks --
time_t t;
for (t=start_gps; t<end_gps; t+=delta) {
    rc = daq_recv_next(&daqd);
    if (rc) {
        printf("Receive data failed with error: %s\n", daq_strerror(rc));
        return 4;
    }

    //--  Get data --
		    chan_req_t* stat = daq_get_channel_status(&daqd, channel[0]);
		    if (!stat || stat->status <= 0) break;
				*length = stat->status
				//int daq_get_scaled_data(daq_t *daq, const char* channel, float* data)
				int	daq_get_scaled_data(daq_t &daqd, channel[0], data)
}
//--  Disconnect from server --
daq_disconnect(&daqd);

}


/**  \mainpage NDS2 Application Program Interface
  *  This module defines the client interface to both versions of the Network
  *  Data Server (classic NDS and NDS2). It is based on the "daq_" function 
  *  set originally defined by Alex Ivanov for the classic NDS. The C-functions
  *  provide a low level interface to the server. In general, these must be
  *  used to implement the client server protocol defined elsewhere, e.g.
  *  <a href="http://www.ligo.caltech.edu/~jzweizig/dmt/IO/group__IO__daqs">
  *  here</a>. A new set of client functions maintain a list of channels to
  *  be requested, properly formats the NDS server commands and help retrieve
  *  the requested data.
  *
  *
  *  The API is subdivided into the following modules:
  *  - \ref daq2_api Access functions and control/status structures.
  *  - \ref daq2_listener Listener thread (deprecated)
  *  - \ref daq2_access_rc Return codes used by the functions.
  *  - \ref daq2_internal Internal and obsolete stuff.
  *  \{
  */


