#include "gstypes.h"
#include <stdio.h>
#include "strings.h"

/*  The idea here is to treat datasets as seperate objects, which SHOULD:
 *     - allow easier reuse of data for different attributes.
 *     - allow a mechanism for changing data and have changes reflected
 *       in each attribute using that data.
 *     - allow a mechanism to automatically update data when the data source
 *       is changed.
 *     - allow easier weaning from GRASS.
 *     - allow easier use of shared memory between processes.
 * 
 * These structures are defined in gstypes.h:
 * typedef struct{
 *     float *fb;
 *     int *ib;
 *     short *sb;
 *     char *cb;
 *     struct BM *bm;
 * } typbuff;
  How about adding a transform func here, so GET_MAPATT would do an
  on-the-fly transformation? Or even a transform func LIST!
 * 
 * typedef struct{
 *     int data_id;
 *     int dims[MAXDIMS];
 *     int ndims;
 *     int numbytes;
 *     char unique_name[80];
 *     typbuff databuff;
 *     int changed;
 *     int need_reload;
 * } dataset;
 *
 * 
*/

/*
#define TRACE_FUNCS
*/

#define LUCKY 33
#define BLOC 20
#define MAX_DS 100 
static dataset *Data[MAX_DS];
static dataset Ds[MAX_DS];  /* trying to avoid allocation */

static int Numsets = 0; 

static int Cur_id = LUCKY; 
static int Cur_max; 
static int Tot_mem = 0; 

/******************************************************************/
static int
init_gsds()
{
int i;


    for(i=0; i<MAX_DS; i++){  /* avoiding dynamic allocation */
	Data[i] = &(Ds[i]);
    }

    Cur_max = MAX_DS;
    return (1);
}


/******************************************************************/
static int
check_numsets()
{
int siz;

    if(Numsets < Cur_max) return (0);

    fprintf(stderr,"maximum number of datasets exceeded\n");
    exit(0);
}

/******************************************************************/
/* returns handle to gsds.   
   successive calls will continue search until "begin" is set   
   (problem here is, unique_name no longer uniquely identifies
   dataset, since changes may be made; but unique_name should still
   be useful for reloading dataset)
   changes & types are set to actual for dataset if found.
*/
extern int
gsds_findh(name, changes, types, begin)
char *name;
IFLAG *changes, *types;  /* acceptable changes & types, flags may be or'd */
		     /* not changed is assumed to always be acceptable */
int begin;           /* flag to indicate search from beginning */
{
static int i;
int start;
    
    start = begin? 0: i+1;
    for(i=start; i<Numsets; i++){
	if(!strcmp(Data[i]->unique_name, name)){ 
	    if((Data[i]->changed & *changes) || !(Data[i]->changed)){
		if(get_type(Data[i]) & *types){
		    *changes = Data[i]->changed;
		    *types = get_type(Data[i]);
		    return(Data[i]->data_id);
		}
	    }
	}
    }
    return(-1);

}



/******************************************************************/
/* returns handle to gsds */
extern int
gsds_newh(name)
char *name;
{
dataset *new;
static int first=1;
int i;
    
    if (first){
	if(0 > init_gsds()) return(-1);
	first = 0;
    }
    else if(0 > check_numsets()) return(-1);

    if (!name) return(-1);

    new = Data[Numsets]; 

    if(new){
	Numsets++;
	new->data_id = Cur_id++;
	for (i=0; i<MAXDIMS; i++){
	    new->dims[i] = 0;
	}
	strcpy(new->unique_name, name);
	new->databuff.fb = NULL;
	new->databuff.ib = NULL;
	new->databuff.sb = NULL;
	new->databuff.cb = NULL;
	new->databuff.bm = NULL;
	new->databuff.nm = NULL;
	new->databuff.k  = 0.0;
	new->changed = 0;
	new->ndims = 0;
	new->need_reload = 1;
	return(new->data_id);
    }
    return(-1);
}


/******************************************************************/
static dataset 
*get_dataset(id)
int id;
{
int i;

    for(i=0; i<Numsets; i++){
	if(Data[i]->data_id == id) 
	    return(Data[i]);
    }
    return(NULL);
}


/******************************************************************/
/* change_flag just tells us to set changed flag - doesn't prevent
writing a buff thats's been gotten with change_flag == 0 (could return a
copy, but willing to trust calling func for now) */

extern typbuff
*gsds_get_typbuff(id, change_flag)
int id; 
IFLAG change_flag;
{
dataset *ds;

    if(ds = get_dataset(id)){
	ds->changed = ds->changed | change_flag;
	ds->need_reload = 0;
	return(&(ds->databuff));
    }
    return(NULL);
}

/******************************************************************/
extern char
*gsds_get_name(id)
int id;
{
int i;
dataset *fds;
static char retstr[160];

    for(i=0; i<Numsets; i++){
	if(Data[i]->data_id == id){
	    fds = Data[i];
	    strcpy (retstr,fds->unique_name);
	    return(retstr);
	}
    }
    return(NULL);
}


/******************************************************************/
extern int
gsds_free_datah(id)
int id;
{
int i, j, found = 0;
dataset *fds;

#ifdef TRACE_FUNCS
fprintf(stderr,"gsds_free_datah\n");
#endif

    for(i=0; i<Numsets; i++){
	if(Data[i]->data_id == id){
	    found = 1;
	    fds = Data[i];
	    free_data_buffs(fds, ATTY_ANY);
	    strcpy(fds->unique_name, "");
	    fds->data_id = 0;
	    for(j=i; j<(Numsets-1); j++)
		Data[j] = Data[j+1];
	    Data[j] = fds;
	}
    }
    if(found) --Numsets;
    return(found);
}


/******************************************************************/
extern int
gsds_free_data_buff(id, typ)
int id, typ;
{
int i, found=0;
dataset *fds;

    for(i=0; i<Numsets; i++){
	if(Data[i]->data_id == id){
	    found = 1;
	    fds = Data[i];
	    free_data_buffs(fds, typ);
	}
    }
    return(found);

}

/******************************************************************/
int
free_data_buffs(ds, typ)
dataset *ds;
int typ;
{
int nsiz=1, i, siz, freed=0;

    for(i=0; i<ds->ndims; i++){
	nsiz *= ds->dims[i];	    
    }

    if(typ & ATTY_NULL){
	if(ds->databuff.nm) {
	    siz = BM_get_map_size(ds->databuff.nm);
	    BM_destroy(ds->databuff.nm);
	    ds->databuff.nm = NULL;
	    freed += siz;
	}
    }
    if(typ & ATTY_MASK){
	if(ds->databuff.bm) {
	    siz = BM_get_map_size(ds->databuff.bm);
	    BM_destroy(ds->databuff.bm);
	    ds->databuff.bm = NULL;
	    freed += siz;
	}
    }
    if(typ & ATTY_CHAR){
	if(ds->databuff.cb) {
	    siz = nsiz * sizeof(char);	    
	    free(ds->databuff.cb);
	    ds->databuff.cb = NULL;
	    freed += siz;
	}
    }
    if(typ & ATTY_SHORT){
	if(ds->databuff.sb) {
	    siz = nsiz * sizeof(short);	    
	    free(ds->databuff.sb);
	    ds->databuff.sb = NULL;
	    freed += siz;
	}
    }
    if(typ & ATTY_INT){
	if(ds->databuff.ib) {
	    siz = nsiz * sizeof(int);	    
	    free(ds->databuff.ib);
	    ds->databuff.ib = NULL;
	    freed += siz;
	}
    }
    if(typ & ATTY_FLOAT){
	if(ds->databuff.fb) {
	    siz = nsiz * sizeof(float);	    
	    free(ds->databuff.fb);
	    ds->databuff.fb = NULL;
	    freed += siz;
	}
    }

    Tot_mem -= freed;
    ds->numbytes -= freed;

#ifdef DEBUG_MSG
    if(freed){
	fprintf(stderr,"freed data from id no. %d\n", ds->data_id);
	fprintf(stderr, "%.3f Kbytes freed, current total = %.3f\n", 
		freed/1000., Tot_mem/1000.);
    }
#endif
    return(freed);

}


/******************************************************************/
/* allocates correct buffer according to type, keeps track of total mem */
/* TODO: add ATTY_CONST */

int
gsds_alloc_typbuff(id, dims, ndims, type)
int id, *dims, ndims, type;
{
dataset *ds;
int i, siz=1;

    if(ds = get_dataset(id)){
/*
	free_data_buffs(ds); 
	careful here - allowing > 1 type to coexist (for float -> color conv.)
	now also use this to allocate a null mask
	(then if not used, use gsds_free_data_buff(id, ATTY_NULL))
*/
	for(i=0; i<ndims; i++){
	    ds->dims[i] = dims[i];
	    siz *= dims[i];	    
	}
	switch(type){
	    case ATTY_NULL:
		if(ndims != 2)  /* higher dimension bitmaps not supported */
		    return(-1);
		if(NULL == (ds->databuff.nm = BM_create(dims[1], dims[0])))
		    return(-1);
		siz = BM_get_map_size(ds->databuff.nm);
		break;
	    case ATTY_MASK:
		if(ndims != 2)  /* higher dimension bitmaps not supported */
		    return(-1);
		if(NULL == (ds->databuff.bm = BM_create(dims[1], dims[0])))
		    return(-1);
		siz = BM_get_map_size(ds->databuff.bm);
		break;
	    case ATTY_CHAR:
		siz *= sizeof(char);
		if(siz){
		    if(NULL == (ds->databuff.cb = (char *)malloc(siz)))
			return(-1);
		}
		else 
		    return(-1);
		break;
	    case ATTY_SHORT:
		siz *= sizeof(short);
		if(siz){
		    if(NULL == (ds->databuff.sb = (short *)malloc(siz)))
			return(-1);
		}
		else 
		    return(-1);
		break;
	    case ATTY_INT:
		siz *= sizeof(int);
		if(siz){
		    if(NULL == (ds->databuff.ib = (int *)malloc(siz)))
			return(-1);
		}
		else 
		    return(-1);
		break;
	    case ATTY_FLOAT:
		siz *= sizeof(float);
		if(siz){
		    if(NULL == (ds->databuff.fb = (float *)malloc(siz)))
			return(-1);
		}
		else 
		    return(-1);
		break;
	    default:
		return(-1);
		break;
	}
	ds->changed = 0;  /* starting with clean slate */
	ds->need_reload = 1;
	ds->numbytes += siz;
	ds->ndims = ndims;
	Tot_mem += siz;

#ifdef DEBUG_MSG
fprintf(stderr, "%f Kbytes allocated, current total = %f\n", 
		siz/1000., Tot_mem/1000.);
#endif

	return(siz);
    }
    return(-1);
}


/******************************************************************/
extern int
gsds_get_changed(id)
int id;
{
dataset *ds;

    if(ds = get_dataset(id)){
	return((int)ds->changed);
    }
    return(-1);

}


/******************************************************************/
extern int
gsds_set_changed(id, reason)
int id; 
IFLAG reason;
{
dataset *ds;

    if(ds = get_dataset(id)){
	ds->changed = reason;
    }
    return(-1);
}


/******************************************************************/
static int
get_type(ds)
dataset *ds;
{
    
    if(ds){
	if(ds->databuff.bm)   return(ATTY_MASK);
	if(ds->databuff.cb)   return(ATTY_CHAR);
	if(ds->databuff.sb)   return(ATTY_SHORT);
	if(ds->databuff.ib)   return(ATTY_INT);
	if(ds->databuff.fb)   return(ATTY_FLOAT);
    }
    return(-1);
}

/******************************************************************/
extern int 
gsds_get_type(id)
int id;
{
dataset *ds;

    ds = get_dataset(id);
    return(get_type(ds));
}


/******************************************************************/
/******************************************************************/







