/*
Written by Bill Brown, August 1994
Calibration of data is new in HDF3.2 & later - this is
needed to import Pathfinder data 
*/


#include "df.h"
#include "hdf.h"

get_converted_sds_data(hdf_file, ndims, dims, data)
char *hdf_file;
int32 ndims, dims[];
float32 *data;
{
int32 numtype;
double cal, cal_err, ioff, ioff_err;
int32 cal_type;
int ret, numelems, i, docal=1;

    DFSDgetNT(&numtype);
    if(FAIL == DFSDgetcal(&cal, &cal_err, &ioff, &ioff_err, &cal_type))
	docal = 0;
    else if(cal == 1.0 && ioff == 0.0) 
	docal = 0;

    numelems = dims[0] * dims[1];

/*
FROM the HDF Documentation:
The relationship between a value 'iy' stored in an SDS and the
actual value 'y' is defined as:

    y = cal * (iy - ioff)
*/

    switch(numtype){
            case DFNT_UCHAR:
	    {
		uchar8 *gdat;
		    
		    if(NULL == 
			(gdat =(uchar8 *)malloc(numelems*SIZE_UCHAR))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] - ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);
	    }
            case DFNT_CHAR: 
	    {
		char8 *gdat;
		    
		    if(NULL == 
			(gdat =(char8 *)malloc(numelems*SIZE_CHAR))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_UINT8:
	    {
		uint8 *gdat;
		    
		    if(NULL == 
			(gdat =(uint8 *)malloc(numelems*SIZE_UINT8))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_INT8:
	    {
		int8 *gdat;
		    
		    if(NULL == 
			(gdat =(int8 *)malloc(numelems*SIZE_INT8))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_INT16:
	    {
		int16 *gdat;
		    
		    if(NULL == 
			(gdat =(int16 *)malloc(numelems*SIZE_INT16))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_UINT16:
	    {
		uint16 *gdat;
		    
		    if(NULL == 
			(gdat =(uint16 *)malloc(numelems*SIZE_UINT16))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_INT32:
	    {
		int32 *gdat;
		    
		    if(NULL == 
			(gdat =(int32 *)malloc(numelems*SIZE_INT32))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_UINT32:
	    {
		uint32 *gdat;
		    
		    if(NULL == 
			(gdat =(uint32 *)malloc(numelems*SIZE_UINT32))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_FLOAT32:
	    {
		    ret = DFSDgetdata(hdf_file, 2, dims, data);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (data[i] -ioff);
		    return(ret);

	    }
            case DFNT_FLOAT64:
	    {
		float64 *gdat;
		    
		    if(NULL == 
			(gdat =(float64 *)malloc(numelems*SIZE_FLOAT64))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetdata(hdf_file, 2, dims, gdat);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);
	    }
	    default:
		fprintf(stderr,"Unknown Number Type.\n");
		return(-1);

    }

}

get_converted_sds_dataslice(fn, winst, windims, data, slicedims)
char *fn;
int32 *winst, *windims, *slicedims;
float32 *data;
{
int32 numtype;
double cal, cal_err, ioff, ioff_err;
int32 cal_type;
int ret, numelems, i, docal=1;


    DFSDgetNT(&numtype);
    if(FAIL == DFSDgetcal(&cal, &cal_err, &ioff, &ioff_err, &cal_type))
	docal = 0;
    else if(cal == 1.0 && ioff == 0.0) 
	docal = 0;

#ifdef DEBUG
{
static int first=1;
if(first){
    first=0;
    fprintf(stderr,"cal = %.6lf ioff = %.6lf\n", cal, ioff);
    fprintf(stderr,"cal_err = %.6lf ioff_err = %.6lf\n", cal_err, ioff_err);
    fprintf(stderr,"cal_type = %ld\n\n", cal_type);
}
}
#endif

    numelems = slicedims[1];


    switch(numtype){
            case DFNT_UCHAR:
	    {
		uchar8 *gdat;
		    
		    if(NULL == 
			(gdat =(uchar8 *)malloc(numelems*SIZE_UCHAR))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] - ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_CHAR: 
	    {
		char8 *gdat;
		    
		    if(NULL == 
			(gdat =(char8 *)malloc(numelems*SIZE_CHAR))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_UINT8:
	    {
		uint8 *gdat;
		    
		    if(NULL == 
			(gdat =(uint8 *)malloc(numelems*SIZE_UINT8))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_INT8:
	    {
		int8 *gdat;
		    
		    if(NULL == 
			(gdat =(int8 *)malloc(numelems*SIZE_INT8))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_INT16:
	    {
		int16 *gdat;
		    
		    if(NULL == 
			(gdat =(int16 *)malloc(numelems*SIZE_INT16))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_UINT16:
	    {
		uint16 *gdat;
		    
		    if(NULL == 
			(gdat =(uint16 *)malloc(numelems*SIZE_UINT16))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_INT32:
	    {
		int32 *gdat;
		    
		    if(NULL == 
			(gdat =(int32 *)malloc(numelems*SIZE_INT32))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_UINT32:
	    {
		uint32 *gdat;
		    
		    if(NULL == 
			(gdat =(uint32 *)malloc(numelems*SIZE_UINT32))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);

	    }
            case DFNT_FLOAT32:
	    {
		    ret = DFSDgetslice(fn, winst, windims, data, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (data[i] -ioff);
		    return(ret);

	    }
            case DFNT_FLOAT64:
	    {
		float64 *gdat;
		    
		    if(NULL == 
			(gdat =(float64 *)malloc(numelems*SIZE_FLOAT64))){
			fprintf(stderr,"Out of memory!\n");
			return(-1);
		    }
		    ret = DFSDgetslice(fn, winst, windims, gdat, slicedims);
		    if(docal)
			for(i=0; i<numelems; i++)
			    data[i] = cal * (gdat[i] -ioff);
		    else
			for(i=0; i<numelems; i++)
			    data[i] = (float32) gdat[i];
		    free(gdat);
		    return(ret);
	    
	    }
	    default:
		fprintf(stderr,"Unknown Number Type.\n");
		return(-1);

    }


}
