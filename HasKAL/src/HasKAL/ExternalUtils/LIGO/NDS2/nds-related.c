
#include "daqc.h"
#include "channel.h"
#include "daqc_response.h"
#include "nds.h"
#include "nds_logging.h"
#include "trench.h"
#include <stdio.h>
#include <stdlib.h>
//#include "nds-related.h"

#define MAX_HOST_LENGTH 256


void nds_GetData (const char* server, int port, const char* channel[], int nch, int start_gps_in, int end_gps_in, int delta_in, float* data, int* length) {

//-- Initialize --
int rc = daq_startup();
if (rc) {
   printf("global initialization failed\n");
}

//-- Connect to server --
daq_t daqd;
rc = daq_connect(&daqd, server, port, nds_v2);
if (rc) {
    printf("Connection failed with error: %s\n", daq_strerror(rc));
}

int i;
for (i=0; i<nch; i++) {
    rc = daq_request_channel(&daqd, channel+i, 0, 0);
}

//--  Request data --
time_t start_gps = start_gps_in;
time_t end_gps   = end_gps_in;
time_t delta     = delta_in;
rc = daq_request_data(&daqd, start_gps, end_gps, delta);
if (rc) {
   printf("Data request failed with error: %s\n", daq_strerror(rc));
}

//--  Read data blocks --
time_t t;
for (t=start_gps; t<end_gps; t+=delta) {
    rc = daq_recv_next(&daqd);
    if (rc) {
       printf("Receive data failed with error: %s\n", daq_strerror(rc));
    }

    //--  Get data --
    int ic;
    for (ic=0;ic<nch;ic++) {
        chan_req_t* stat = daq_get_channel_status(&daqd, channel[ic]);
        if (!stat || stat->status <= 0) break;
        *length = stat->status;
        daq_get_scaled_data(&daqd, channel[ic], data);
    }
}

//--  Disconnect from server --
daq_disconnect(&daqd);
}


void nds_GetChannels (const char* server, int port, int gps, int* num_channels, daq_channel_t* channels) {
/* Local variable declaration */
//short port = 31200;
daq_t daq;
int err;
int num_alloc    = 0;
channels = NULL;
//num_channels = 0;
chantype_t chant = cUnknown;

//-- Initialize --
int rc = daq_startup();
if (rc) {
   printf("global initialization failed\n");
}

//-- Connect to server --
daq_t daqd;
rc = daq_connect(&daqd, server, port, nds_v2);
if (rc) {
    printf("Connection failed with error: %s\n", daq_strerror(rc));
}

//--  Request channel list --
/*---  Get the number of channels */
err = daq_recv_channel_list(&daq, channels, 0, &num_alloc, gps, chant);
if (err) {
   daq_recv_shutdown(&daq);
   print("get_channel_list() failed.");
}

/*---  Read in the channel list */
if (num_alloc > 0) {
   channels = calloc(num_alloc, sizeof(daq_channel_t));
   if (!channels) {
      printf("Channel list calloc() failed.");
    }
   err = daq_recv_channel_list(&daq, channels, num_alloc, &num_channels, gps, chant);
   if (err) {
      printf("get_channel_list() failed.");
   } else if (!num_channels) {
     free(channels);
     channels = NULL;
    }
}
/*---  Close the connection */
daq_disconnect(&daq);
daq_recv_shutdown(&daq);


}

//
//
// nds parameters
//
// const char* server = "ldas-pcdev1.ligo.caltech.edu";
//
// int port = 31200;
//
// const char* channel[4] = {
//     "H1:DMT-STRAIN",
//     "L1:DMT-STRAIN",
//     "V1:h16384Hz",
//     0
// }
