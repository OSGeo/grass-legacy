
/* the caps are being built by reading thru the original grid3 file 
** and drawn to the screen not being stored at this time
*/
#include "Viz.h"
/*
#define DEBUG1
*/

init_caps(g3file,D_Cap,G3header)
char *g3file;
struct Cap *D_Cap;
file_info *G3header;
/* this subroutine only needs to be called once */
{
   int		min; /*the smallest dimension*/
   int		dim1,dim2; /* the two largest dimensions */
   int		ret;

/* open grid3 file for reading */

    if((G3header->datainfp = fopen(g3file,"r")) == NULL)
    {
	fprintf(stderr,"ERROR:  unable to open %s for reading\n",g3file);
	return(-1);
    }
	
/* read info into G3header structure */    
    ret = g3read_header(G3header);
    if(ret <  0 )
	fprintf(stderr,"ERROR in reading grid3 file\n");	

    D_Cap->offset = ftell(G3header->datainfp); 


/* In order to determine how much memory to malloc for a D_buff
** need to determine the largest 2 dimensions (x,y or z)
** this is done by finding the minimum and using the other two */
    if (G3header->xdim < G3header->ydim)
    {
	dim1 = G3header->ydim;
	min = G3header->xdim;
    }
    else
    {
	dim1 = G3header->xdim;
	min = G3header->ydim;
    }
    
    if (G3header->zdim < min)
	dim2 = min;
    else
	dim2 = G3header->zdim;
    
/* NOTE: code only written for floats at this time */
/* malloc memory for buffer that will hold slice of data */
    if ((D_Cap->D_buff = (float *)malloc(dim1*dim2*sizeof(float))) == NULL)
    {
	fprintf(stderr,"ERROR: in mallocing memory for D_Cap->D_buff\n");
	return(-1);
    }
    return(1);

}

