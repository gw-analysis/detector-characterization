//
//
//

#include "daqc.h"
#include "channel.h"
#include "daqc_response.h"
#include "nds.h"
#include "nds_logging.h"
#include "trench.h"
#include <stdio.h>

int nds_GetData (const char* server, int port, const char* channel[], int start_gps_in, int end_gps_in, int delta_in) {


//-- Initialize --
int rc = daq_startup();
if (rc) {
    printf("global initialization failed\n");
    return 1;
}

//-- Connect to server --
//const char* server = "ldas-pcdev1.ligo.caltech.edu";
//int           port = 31200;
daq_t daqd;
rc = daq_connect(&daqd, server, port, nds_v2);
if (rc) {
    printf("Connection failed with error: %s\n", daq_strerror(rc));
    return 2;
}

//-- Specify channels --
//const char* channel[4] = {
//    "H1:DMT-STRAIN",
//    "L1:DMT-STRAIN",
//    "V1:h16384Hz",
//    0
//};

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
    chan_req_t* stat = daq_get_channel_status(&daqd, channel[1]);
    if (!stat || stat->status <= 0) break;
}
//--  Disconnect from server --
daq_disconnect(&daqd);

}




