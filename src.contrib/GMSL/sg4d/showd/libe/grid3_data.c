#include "viz.h"
#include <stdio.h>
#include <math.h>
#include <unistd.h>

#define	XDIMYDIM	(Headfax->xdim)*(Headfax->ydim)

g3read_level (Headfax, data, n_lev)
    file_info *Headfax;
    float     *data;
    int        n_lev;
{
    static size_t	datasize;
    static int		dataflag = 1;
    static int		mallocflag = 1;
    static short 	*shortdata;/*must be static or will need to remalloc */
    static int		*intdata;/* must be static or will need to remalloc */
    int			x,y;
    int 		num;

    /* what kind of data is in file ? */ 
    if (dataflag)
    {
	if (Headfax->type == 1)
	    datasize = sizeof(short int);
	else if(Headfax->type ==2)
	    datasize = sizeof(int);
	else if (Headfax->type == 3)
	    datasize = sizeof(float); 
	else
	    return(-1);/* error in header information */
    dataflag = 0;
    }
    if (fseek(Headfax->datainfp,
	(long)(n_lev*datasize*XDIMYDIM + Headfax->headsize),0) != 0)
             return(-1);

    switch(Headfax->type)
    {
	case 1:/*data must be converted to floats one at a time */
	    if (mallocflag)
	    {
		if((shortdata =
			  (short int*)malloc(datasize*XDIMYDIM)) == NULL)
		{
		    fprintf(stderr,"Error not enough memory\n");
		    return(-1);
		}
		mallocflag = 0;
	    }
	    num = fread(shortdata,datasize,XDIMYDIM,Headfax->datainfp);

	    if (num != XDIMYDIM)

	    {
		fprintf(stderr,
		"Error in assigning info from file \n" );
		return(-1);
	    }
	    for(y=0; y<Headfax->ydim; y++)
		for(x=0; x<Headfax->xdim; x++)
		{
		    data[y*Headfax->xdim +x] =
				  (float)shortdata[y*Headfax->xdim + x];
		}
		
		break;


	case 2:/*data must be converted to floats one at a time */
	    if (mallocflag)
	    {
		if((intdata =
			  (int*)malloc(datasize*XDIMYDIM)) == NULL)
		{
		    fprintf(stderr,"Error not enough memory\n");
		    return(-1);
		}
		mallocflag = 0;
	    }
	    num = fread(intdata,datasize,XDIMYDIM,Headfax->datainfp);

	    if (num != XDIMYDIM){
		fprintf(stderr,
		"Error in assigning info from file \n" );
		return(-1);
	    }
	    for(y=0; y<Headfax->ydim; y++)
		for(x=0; x<Headfax->xdim; x++)
		    data[y*Headfax->xdim +x] =
					(float)intdata[y*Headfax->xdim+x];
		    
	    break;
	case 3: /* data is already floats */
	    num = fread(data,datasize,XDIMYDIM,(Headfax->datainfp));

	    if (num != XDIMYDIM)
	    {
		fprintf(stderr,
		"Error in assigning info from file \n" );
		return(-1);
	    }


	
    }
    return 0;
}





g3put_level (Headfax, data)
    file_info *Headfax;
    VOID_TYPE     *data;
{
    static size_t	datasize;
    static int		dataflag = 1;
    static int		mallocflag = 1;
    int			x,y;
    int 		i,j,num;
    short int           datsint;
    int                 datint;
    float               datfloat;
    /* what kind of data goes to the file ? */ 
    switch(Headfax->type)
    {
	case 1:
          datasize = sizeof(short int);
          break;
	case 2:
          datasize = sizeof(int);
          break;
	case 3:				/* looks like a bug -dpg 6/93 (float?)*/
          datasize = sizeof(int);
          break;
        default:
          return(-1);/* error in header information */
          break;
     }

    if((num=fwrite(data,datasize,XDIMYDIM,
	    (Headfax->dataoutfp))) !=  XDIMYDIM)
    {
        fprintf(stderr,"ERROR: in writing data to file \n");
        return(-1);
    }

    return(1);      
}
