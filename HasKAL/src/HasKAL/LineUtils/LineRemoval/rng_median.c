/****************************************************
 *rng_median.h
 *Created:2014/06/17
 *Author:Mitsuhiro Asano
 ****************************************************
 */
//Last Modified: 2014/08/28 09:12:09

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int array_comp(const void* x,const void* y) {
  const double* a;
  const double* b;
  a = x;
  b = y;
  if((*(double**)a)[1] > (*(double**)b)[1] )return 1;
  if((*(double**)a)[1] < (*(double**)b)[1] )return -1;
  
  if(*a>*b)return 1;
  if(*a<*b)return -1;
  return 0;
}

int double_comp(const void* x,const void* y) {
  const double* a=(const double*)x;
  const double* b=(const double*)y;
  if(*a>*b)return 1;
  if(*a<*b)return -1;
  return 0;
}

double simple_med(double *data,int lendata){
  double simple_median;
  qsort(data,lendata,sizeof(double),double_comp);
  simple_median = data[(int)(lendata/2)];
  return simple_median;
}

void rng_med(double *data,int lendata,int nblocks,double *after_med){

  int idx;
  double *medians;//,*block;
  double sorted_indices[nblocks];
  double **for_sort,**from_sort;

  struct node{
    double data;
    struct node *next_sorted,*next_sequence,*prev_sorted;
    int rank;
  };

  struct node **checks,**node_addresses;
  struct node *first_sequence,*last_sequence;
  struct node *currentnode,*previousnode;
  struct node *leftnode,*rightnode;
  struct node *reuse_next_sorted,*reuse_prev_sorted;
  struct node *dummy_node,*dummy_node1,*dummy_node2;
  int ncheckpts,stepchkpts;
  int nextchkptindx,*checks4shift;
  int nearestchk,midpoint,offset,numberoffsets;
  int samplecount,k,counter_chkpt,chkcount,shiftcounter;
  double nextsample,deletesample,dummy;
  int shift,dummy_int;

  //memory

  for_sort = (double**)malloc(sizeof(double*)*nblocks);
  from_sort = (double**)malloc(sizeof(double*)*nblocks);
  for (idx=0 ; idx< nblocks ; idx++){
    for_sort[idx] = (double*)malloc(sizeof(double)*2);
    from_sort[idx] = (double*)malloc(sizeof(double)*2);
  }

  //
  //Sort the first block of nblocks sample
  //

  for(idx=0 ;idx<nblocks ; idx++ ){
    for_sort[idx][0] = idx;
    for_sort[idx][1] = data[idx];
  }

  qsort(for_sort,nblocks,sizeof(double*),array_comp);
  for(idx=0 ;idx<nblocks ; idx++ ){
    from_sort[idx][0] = for_sort[idx][0];
    from_sort[idx][1] = for_sort[idx][1];
    sorted_indices[idx] = from_sort[idx][0];
    //    printf("%le %le\n",sorted_indices[idx],from_sort[idx][1]);
  }

  //from_sort[idx][1] : data after sorted
  //from_sort[idx][0] : index after sorted

  //
  //Indices of checkpoint nodes.
  //

  stepchkpts = sqrt(nblocks);
  ncheckpts=nblocks/stepchkpts;
  checks = (struct node **)malloc(sizeof(struct node*)*ncheckpts);
  if(!checks){
    printf("Could not allocate storage for checks\n");
  }
  if(!(checks4shift = (int*)malloc(sizeof(int)*ncheckpts))){
    printf("Could not allocate storage for checks4shift\n");
  }

  //
  //nblocks is odd or even.
  // 

  if((int)fmod(nblocks,2.0)){  //odd
    midpoint=(nblocks+1)/2-1;
    numberoffsets=1;
  }
  else{  //even
    midpoint=nblocks/2-1;
    numberoffsets=2;
  }
  nearestchk=floor(midpoint/stepchkpts);
  offset=midpoint-nearestchk*stepchkpts;

  //
  //Build up linked list using first nblock points
  // 

  node_addresses = (struct node **)malloc(sizeof(struct node*)*nblocks);
  if(!node_addresses){
    printf("Could not allocate storage for node_addresses\n");
  }
  first_sequence = (struct node *)malloc(sizeof(struct node)*1);
  if(!first_sequence){
    printf("Could not create firsr_sequence \n");
  }
  node_addresses[0]=first_sequence;
  first_sequence->next_sequence=NULL;
  first_sequence->next_sorted=NULL;
  first_sequence->prev_sorted=NULL;
  first_sequence->data=data[0];
  previousnode=first_sequence;
  for(samplecount=1 ; samplecount<nblocks ; samplecount++){  
    currentnode = (struct node *)malloc(sizeof(struct node)*1);
    if(!currentnode){
      printf("Could not create firsr_sequence \n");
    }
    node_addresses[samplecount]=currentnode;
    previousnode->next_sequence=currentnode;
    currentnode->next_sequence=NULL;
    currentnode->prev_sorted=NULL;
    currentnode->next_sorted=NULL;
    currentnode->data=data[samplecount];
    previousnode=currentnode;
  }
  last_sequence=currentnode;

  //
  //Set the sorted sequence pointers and the pointers to checkpoint nodes  
  //
  dummy_node =  (struct node *)malloc(sizeof(struct node)*1);
  currentnode=node_addresses[(int)sorted_indices[0]];
  previousnode=NULL;
  checks[0]=currentnode;
  nextchkptindx=stepchkpts;
  counter_chkpt=1;
  for(samplecount=1 ; samplecount<nblocks ; samplecount++){
    dummy_node=node_addresses[(int)sorted_indices[samplecount]];
    currentnode->next_sorted=dummy_node;
    currentnode->prev_sorted=previousnode;
    previousnode=currentnode;
    currentnode=dummy_node;
    if(samplecount==nextchkptindx && counter_chkpt<ncheckpts){
      checks[counter_chkpt]=currentnode;
      nextchkptindx+=stepchkpts;
      counter_chkpt++;
    }
  }
  currentnode->prev_sorted=previousnode;
  currentnode->next_sorted=NULL;

  //mxDestroyArray(from_sort[0]);
  //mxDestroyArray(from_sort[1]);

  //
  //Allocate storage for output and get the first output element
  //

  medians = (double *)malloc(sizeof(double)*(lendata-nblocks+1));
  if(!medians){
    printf("Could not allocate storage for median\n");
  }
  currentnode=checks[nearestchk];
  for(k=1;k<=offset;k++){
    currentnode=currentnode->next_sorted;
  }
  dummy=0;
  for(k=1;k<=numberoffsets;k++){
    dummy+=currentnode->data;
    currentnode=currentnode->next_sorted;
  }
  medians[0]=dummy/numberoffsets;


  //
  //
  //This is main part. 
  //
  //

  for(samplecount=nblocks;samplecount<lendata;samplecount++){
    nextsample=data[samplecount];
    if(nextsample>=checks[0]->data){
      for(chkcount=1;chkcount<ncheckpts;chkcount++){
	if(nextsample>=checks[chkcount]->data){
	}
	else{
	  break;
	}
      }
      chkcount-=1;
      rightnode=checks[chkcount];
      leftnode=NULL;
      while(rightnode){                                                                                                
	if(nextsample<rightnode->data){
	  break;
	}
	leftnode=rightnode;
	rightnode=rightnode->next_sorted;
      }
    }
    else{
      if(nextsample<checks[0]->data){
	chkcount=0;
	rightnode=checks[0];
	leftnode=NULL;
      }
    }

    //
    //checkpoints need to be shifted
    //

    dummy_node=NULL;
    if(rightnode==first_sequence){
      dummy_node=rightnode;
    }
    else if(leftnode==first_sequence){
      dummy_node=leftnode;
    }
    if(dummy_node) {
      dummy_node->data=nextsample;
      first_sequence=first_sequence->next_sequence;                                                           
      dummy_node->next_sequence=NULL;
      last_sequence->next_sequence=dummy_node;
      last_sequence=dummy_node;
      shift=0;
    }
    else{
      reuse_next_sorted=rightnode;
      reuse_prev_sorted=leftnode;
      shift=1; /*shift maybe required*/
    }

    //
    //shift
    //

    if(shift){
      deletesample=first_sequence->data;
      if(deletesample>nextsample){
	shiftcounter=0;
	for(k=chkcount;k<ncheckpts;k++){
	  dummy=checks[k]->data;
	  if(dummy>nextsample){
	    if(dummy<=deletesample){
	      checks4shift[shiftcounter]=k;
	      shiftcounter++;
	    }
	    else{
	      break;
	    }
	  }
	}
	shift=-1; /*Left shift*/
      }
      else
	if(deletesample<=nextsample){
	  shiftcounter=0;
	  for(k=chkcount;k>=0;k--){
	    dummy=checks[k]->data;
	    if(dummy>=deletesample){
	      checks4shift[shiftcounter]=k;
	      shiftcounter++;
	    }
	    else{
	      break;
	    }
	  }
	  shift=1; /*Shift Right*/
	}
    }

    //
    //Recycle
    //

    if(shift){

      //
      //Reset
      //

      dummy_node=first_sequence;
      first_sequence=dummy_node->next_sequence;
      dummy_node->next_sequence=NULL;
      last_sequence->next_sequence=dummy_node;
      last_sequence=dummy_node;
      dummy_node->data=nextsample;
      dummy_node1=dummy_node->prev_sorted;
      dummy_node2=dummy_node->next_sorted;

      //      
      //Repair
      //

      if(!dummy_node1){
	dummy_node2->prev_sorted=dummy_node1;
      }
      else {
	if(!dummy_node2){
	  dummy_node1->next_sorted=dummy_node2;
	}
	else{
	  dummy_node1->next_sorted=dummy_node2;
	  dummy_node2->prev_sorted=dummy_node1;
	}
      }

      if(!rightnode){
	leftnode->next_sorted=dummy_node;
      }
      else {
	if(!leftnode){                                                                                              
	  rightnode->prev_sorted=dummy_node;
	}
	else{
	  leftnode->next_sorted=dummy_node;
	  rightnode->prev_sorted=dummy_node;
	}
      }

      //
      //shift check
      //

      if(shift==-1){
	for(k=0;k<shiftcounter;k++){
	  dummy_int=checks4shift[k];
	  checks[dummy_int]=checks[dummy_int]->prev_sorted;
	}
      }
      else
	if(shift==1){
	  for(k=0;k<shiftcounter;k++){
	    dummy_int=checks4shift[k];
	    checks[dummy_int]=checks[dummy_int]->next_sorted;
	  }
	}
      
      dummy_node->next_sorted=reuse_next_sorted;
      dummy_node->prev_sorted=reuse_prev_sorted;

    }

    //
    //Get the median
    //

    currentnode=checks[nearestchk];
    for(k=1;k<=offset;k++){
      currentnode=currentnode->next_sorted;
    }
    dummy=0;
    for(k=1;k<=numberoffsets;k++){
      dummy+=currentnode->data;
      currentnode=currentnode->next_sorted;
    }
    medians[samplecount-nblocks+1]=dummy/numberoffsets;

  }//Large loop closed

  for(idx=0 ; idx<nblocks ; idx++ ){
    free(for_sort[idx]);
    free(from_sort[idx]);
  }
  free(for_sort);
  free(from_sort);
  free(node_addresses);
  free(checks);
  free(checks4shift);
  free(first_sequence);

  while(currentnode){
    previousnode = currentnode;
    currentnode = currentnode->next_sequence;
  }

  free(currentnode);
  free(dummy_node);

  //output is median[] :array(array size is lendata-2*nblocks)

  int endblk;
  endblk = (int)(nblocks/2);

  double dummy_med_first,dummy_med_last;
  double dummy_first[endblk],dummy_last[endblk];

  for(idx=0 ;idx<endblk ; idx++){
    dummy_first[idx] = data[idx];
    dummy_last[idx] = data[lendata - endblk + idx];
  }
  dummy_med_first = simple_med(dummy_first,endblk);
  dummy_med_last = simple_med(dummy_last,endblk);

  //
  //final output
  //

  for(idx=0 ; idx<lendata ; idx++){
    if(idx <  endblk){
      after_med[idx] = dummy_med_first;
    }
    if(endblk < idx && idx < endblk + lendata -nblocks){
      after_med[idx] = medians[idx-endblk+1];
    }
    else{
      after_med[idx] = dummy_med_last;
    }
  } 

  free(medians);

return;

}

