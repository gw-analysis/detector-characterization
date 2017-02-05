
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
#define NEW_VECT(type,dim) ((type*)malloc(dim*sizeof(type)))

void nds_GetData (const char* server, int port, const char* channel[],
  int nch, int start_gps_in, int end_gps_in, int delta_in, float* data,
  int* length) {

chantype_t ctype = 0;
time_t start_gps = start_gps_in;
time_t end_gps   = end_gps_in;
time_t delta     = delta_in;

//-- Initialize --
int rc = daq_startup();
if (rc) {
   printf("global initialization failed\n");
}

//-- Connect to server --
daq_t daqd;
rc = daq_connect(&daqd, server, port, nds_v1);
if (rc) {
    printf("Connection failed with error: %s\n", daq_strerror(rc));
}

int nAlloc = 0;
int nChans = 0;
daq_channel_t* channel_list;
rc = daq_recv_channel_list(&daqd, 0, 0, &nAlloc, start_gps_in, ctype);
channel_list = NEW_VECT(daq_channel_t, (size_t)(nAlloc));
rc = daq_recv_channel_list(&daqd, channel_list, nAlloc,
         &nChans, start_gps, ctype);
if (rc) {
	 printf("Error reading channel list: %s\n", daq_strerror(rc));
} else {
daq_channel_t chan;
int i,j;
for (i=0; i<nch; i++) {
    struct trench_struct tch;
    trench_init(&tch);
    trench_parse(&tch, channel[i]);
    for (j=0; j<nChans; ++j) {
		  if (!trench_cmp_base(&tch, channel_list[j].name)) {
			    trench_infer_chan_info(&tch, ctype,
						   channel_list[j].rate,
						   channel_list[j].data_type);
			    daq_init_channel(&chan, tch.str, tch.ctype,
					     tch.rate, tch.dtype);
			    break;
			}
    }
    daq_request_channel_from_chanlist(&daqd, &chan);
    trench_destroy(&tch);
}

//--  Request data --
rc = daq_request_data(&daqd, start_gps, end_gps, delta);
if (rc) {
   printf("Error in daq_request_data: %s\n", daq_strerror(rc));
}

//--  Read data blocks --
time_t t;
for (t=start_gps; t<end_gps; t+=delta) {
    rc = daq_recv_next(&daqd);
    if (rc) {
       printf("Receive data failed with error: %s\n", daq_strerror(rc));
       return;
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
}


void nds_GetChannels (const char* server,
  int port, int gps, int* num_channels, daq_channel_t* channels) {
/* Local variable declaration */
daq_t daq;
int err;
int num_alloc    = 0;
channels = NULL;
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
   printf("get_channel_list() failed.");
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


void nds_GetNumberOfChannels (const char* server
  , int port, int gps, int* num_alloc) {
/* Local variable declaration */
//short port = 31200;
daq_t daq;
int err;
//daq_channel_t* channels = NULL;
//num_channels = 0;
chantype_t chant = cUnknown;
time_t gpst = gps;

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
err = daq_recv_channel_list(&daqd, 0, 0, num_alloc, gpst, chant);
if (err) {
   daq_recv_shutdown(&daqd);
   printf("get_channel_list() failed.");
}
/*---  Close the connection */
daq_disconnect(&daq);
daq_recv_shutdown(&daq);
}
