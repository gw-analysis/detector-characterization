
#include "daqc.h"
#include "channel.h"
#include "daqc_response.h"
#include "nds.h"
#include "nds_logging.h"
#include "trench.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define MAX_HOST_LENGTH 256
#define NEW_VECT(type,dim) ((type*)malloc(dim*sizeof(type)))
#define SLEEPTIME 100000

void nds_GetData (const char* server, int port, const char* channel[],
  int nch, int start_gps_in, int end_gps_in, int delta_in, float* data,
  int* length) {

int err,err2,err3;
daq_t daqd;
chantype_t ctype = 0;
time_t start_gps = start_gps_in;
time_t end_gps   = end_gps_in;
time_t delta     = delta_in;

//-- treatment of the GPS end time
if (end_gps < start_gps) {
		end_gps += start_gps;
}
if (end_gps < start_gps + delta) {
		delta = end_gps - start_gps;
}

//-- Initialize --
int rc = daq_startup();
if (rc) {
   printf("global initialization failed\n");
}

//-- Connect to server --
err = daq_connect(&daqd, server, port, nds_try);
if (err) {
    usleep(SLEEPTIME);
    err2 = daq_connect(&daqd, server, port, nds_try);
    if (err2) {
       usleep(SLEEPTIME);
       err3 = daq_connect(&daqd, server, port, nds_try);
       if (err3) {
          printf("Connection failed with error: %s\n", daq_strerror(err));
       }
    }
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
time_t t, dt=delta;
for (t=start_gps; t<end_gps; t+=dt) {
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
    dt = (time_t) daq_get_block_secs( &daqd );
}
//--  Disconnect from server --
daq_disconnect(&daqd);
daq_recv_shutdown(&daqd);
}
}


void nds_GetChannels (const char* server,
  int port, int gps, char *name[], double *rate,
  int *tpnum, int *bps, int *chNum, daq_data_t *data_type,
  float *signal_gain, float *signal_slope,
  float *signal_offset, char *signal_units[]) {

/* Local variable declaration */
daq_channel_t *channels;
daq_t daqd;
int err,err2,err3;
int num_alloc    = 0;
int num_channels;
//channels = NULL;
chantype_t chant = cUnknown;
time_t gpst = gps;

//-- Initialize --
err = daq_startup();
if (err) {
   printf("global initialization failed\n");
}

//-- Connect to server --
err = daq_connect(&daqd, server, port, nds_try);
if (err) {
    err2 = daq_connect(&daqd, server, port, nds_try);
    usleep(SLEEPTIME);
    if (err2) {
       err3 = daq_connect(&daqd, server, port, nds_try);
       usleep(SLEEPTIME);
       if (err3) {
          printf("Connection failed with error: %s\n", daq_strerror(err));
       }
    }
}

//--  Request channel list --
/*---  Get the number of channels */
err = daq_recv_channel_list(&daqd, NULL, 0, &num_alloc, gpst, chant);
if (err) {
   daq_recv_shutdown(&daqd);
   printf("get_channel_list() failed.");
}

/*---  Read in the channel list */
if (num_alloc > 0) {
   channels = NEW_VECT(daq_channel_t, (size_t)(num_alloc));
   if (!channels) {
      printf("Channel list calloc() failed.");
    }
   err = daq_recv_channel_list(&daqd, channels, num_alloc,
            &num_channels, gpst, chant);
   if (err) {
      printf("daq_recv_channel_list() failed.");
   } else if (!num_channels) {
     free(channels);
     channels = NULL;
    }
printf("\n");
for (int i=0; i<num_alloc; i++) {
  name[i] = channels[i].name;
  rate[i] = channels[i].rate;
  tpnum[i] = channels[i].tpnum;
  bps[i] = channels[i].bps;
  chNum[i] = channels[i].chNum;
  data_type[i] = channels[i].data_type;
  signal_gain[i] = channels[i].s.signal_gain;
  signal_slope[i] = channels[i].s.signal_slope;
  signal_offset[i] = channels[i].s.signal_offset;
  signal_units[i] = channels[i].s.signal_units;
}
}
/*---  Close the connection */
daq_disconnect(&daqd);
daq_recv_shutdown(&daqd);
}


void nds_GetNumberOfChannels (const char* server
  , int port, int gps, int* num_alloc) {

/* Local variable declaration */
daq_t daqd;
int err,err2,err3;
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
err = daq_connect(&daqd, server, port, nds_try);
if (err) {
    usleep(SLEEPTIME);
    err2 = daq_connect(&daqd, server, port, nds_try);
    if (err2) {
       usleep(SLEEPTIME);
       err3 = daq_connect(&daqd, server, port, nds_try);
       if (err3) {
          printf("Connection failed with error: %s\n", daq_strerror(err));
       }
    }
}

//--  Request channel list --
/*---  Get the number of channels */
err = daq_recv_channel_list(&daqd, 0, 0, num_alloc, gpst, chant);
if (err) {
   daq_recv_shutdown(&daqd);
   printf("get_channel_list() failed.");
}
/*---  Close the connection */
daq_disconnect(&daqd);
daq_recv_shutdown(&daqd);
}


void nds_GetData_stdout (const char* server, int port, const char* channel[],
  int nch, int start_gps_in, int end_gps_in, int delta_in) {

int err,err2,err3;
daq_t daqd;
chantype_t ctype = 0;
time_t start_gps = start_gps_in;
time_t end_gps   = end_gps_in;
time_t delta     = delta_in;
float *data;

//-- treatment of the GPS end time
if (end_gps < start_gps) {
		end_gps += start_gps;
}
if (end_gps < start_gps + delta) {
		delta = end_gps - start_gps;
}

//-- Initialize --
int rc = daq_startup();
if (rc) {
   printf("global initialization failed\n");
}

//-- Connect to server --
err = daq_connect(&daqd, server, port, nds_try);
if (err) {
    usleep(SLEEPTIME);
    err2 = daq_connect(&daqd, server, port, nds_try);
    if (err2) {
       usleep(SLEEPTIME);
       err3 = daq_connect(&daqd, server, port, nds_try);
       if (err3) {
          printf("Connection failed with error: %s\n", daq_strerror(err));
       }
    }
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
//rc = daq_request_data(&daqd, start_gps, end_gps, delta);
//if (rc) {
//   printf("Error in daq_request_data: %s\n", daq_strerror(rc));
//}

//--  Read data blocks --
time_t t;
//time_t dt=delta;
size_t k;
for (t=start_gps; t<end_gps; t+=delta) {
    rc = daq_request_data(&daqd, t, t+delta, delta);
    if (rc) {
       printf("Error in daq_request_data: %s\n", daq_strerror(rc));
    }
    rc = daq_recv_block(&daqd);
    //rc = daq_recv_next(&daqd);
    if (rc) {
       printf("Receive data failed with error: %s\n", daq_strerror(rc));
       return;
    }

    //--  Get data --
    uint4_type ind;
    for (ind=0; ind<nch; ind++) {
       size_t N;
       daq_data_t dtype;
       char* chname;
	     chan_req_t* chan = daqd.chan_req_list + ind;
     	 if (chan->status < 0) {
	       printf("Channel: %s receive error (%i)\n",
	  	   chan->name, -chan->status);
	       continue;
	     }
       chname = chan->name;
       dtype = chan->data_type;
       N = (size_t)chan->status / data_type_size(dtype);
       data =  (float *)malloc(sizeof(float) * N);
       printf("\n");
       printf("Channel: %s  type: %s  nWords: %zi\n",
              chan->name, data_type_name(dtype), N);
       daq_get_scaled_data(&daqd, chname, data);
       /* start stdout */
       printf("\n");
       printf ("%s",chname);
       printf("\n");
       for (k=0;k<N;k++){
          printf("%g", *(data+k));
          printf(" ");
        }
       printf("\n");
       free(data);
    }
//    dt = (time_t) daq_get_block_secs( &daqd );
}
//printf("finish data retrieval\n");
//--  Disconnect from server --
daq_disconnect(&daqd);
//printf("daq_disconnect finished\n");
daq_recv_shutdown(&daqd);
//printf("daq_recv_shutdown finished\n");
}
return;
}
