/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
/* adapted  7/23/90  J Moorman from undxf.c code written by:  
** Programmer: Tom Howard   National Park Service GIS division		    
*/


#include "dxf2vect.h"

double atof();


int 
dxf_add_arc (FILE *dxf_file)
{
    /* DECLARING VARIABLES */
    int  layer_flag = 0;    /* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int  xflag = 0;         /* INDICATES IF A x VALUE HAS BEEN FOUND */
    int  yflag = 0;         /* INDICATES IF A y VALUE HAS BEEN FOUND */
    int  rflag = 0;         /* INDICATES IF A radius HAS BEEN FOUND */		
    int  sflag = 0;	    /* INDICATES IF A start_angle HAS BEEN FOUND */
    int  fflag = 0;	    /* INDICATES IF A finish_angle HAS BEEN FOUND */
    double centerx;         /* READ IN FROM DXF FILE */ 
    double centery;         /* READ IN FROM DXF FILE */   
    double radius;          /* READ IN FROM DXF FILE */  
    float  start_angle;     /* READ IN FROM DXF FILE */
    float  finish_angle;    /* READ IN FROM DXF FILE */
    char *nolayername = "UNIDENTIFIED"; 
    DXF_DIG *layer_fd;         /* POINTER TO LAYER NAME */
    int  code;  /* VARIABLE THAT HOLDS VALUE RETURNED BY readcode() */
	int arr_size = 0;

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */

    while ((code = dxf_readcode(dxf_file)) != 0) 
    {
		if (code == -2)  /* EOF */
			return(0);
		dxf_fgets (dxf_line,256,dxf_file);  
		if (feof(dxf_file) != 0) /* EOF */
			return(0);
		
		switch (code) 
		{
			case   8:
				if(!layer_flag) 
				{
					layer_fd = dxf_which_layer (dxf_line, DXF_ASCII);	
					if (layer_fd == NULL)
					return(0);
					layer_flag = 1;
				}
				break;
			case 10: /* x COORDINATE */
				centerx  = atof(dxf_line);	
				xflag = 1;
				break;
			case 20: /* y COORDINATE */
				centery = atof(dxf_line);	
				yflag = 1;
				break;
			case 30: /* Z COORDINATE NOT BEING USED */
				break;
			case 40: /* RADIUS */
				radius = atof(dxf_line);
				rflag = 1;
				break;
			case 50:
				start_angle = atof(dxf_line);
				sflag = 1;
				break;
			case 51:
				finish_angle = atof(dxf_line);
				fflag = 1;
				break;

			/* THE FOLLOWING GROUPS USED ONLY IF DIFFERENT THAN DEFAULTS */		
			case  38: /* ELEVATION IF NONZERO */
			case  39: /* THICKNESS IF NONZERO */
			case  62: /* COLOR NUMBER (IF NOT "BYLAYER") */
			case 210: /* X EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
			case 220: /* Y EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
			case 230: /* Z EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
			default:
				break;
		}
    }
    if (!layer_flag)
    {
	fprintf(stderr,"Inside !layer_flag");
	layer_fd = dxf_which_layer (nolayername, DXF_ASCII);	
	if (layer_fd == NULL)
	    return(0);
    }

    if (xflag && yflag && rflag && sflag && fflag)
    {
	arr_size = make_arc(0, centerx,centery,radius,start_angle,finish_angle,1);
	write_polylines(layer_fd,arr_size);
    }
    return(1);
}

