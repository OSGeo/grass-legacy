/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
/* adapted  7/23/90  J Moorman from undxf.c code written by:  
** Programmer: Tom Howard   National Park Service GIS division		    
*/
#include <math.h>
#include "dxf2vect.h"
double atof();


int 
dxf_add_circle (FILE *dxf_file)
{
    /* DECLARING VARIABLES */
    int  layer_flag = 0;    /* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int  xflag = 0;         /* INDICATES IF A x VALUE HAS BEEN FOUND */
    int  yflag = 0;         /* INDICATES IF A y VALUE HAS BEEN FOUND */
    int  rflag = 0;         /* INDICATES IF A radius HAS BEEN FOUND */		
    double centerx;         /* READ IN FROM DXF FILE */ 
    double centery;         /* READ IN FROM DXF FILE */   
    double radius;          /* READ IN FROM DXF FILE */  
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
	layer_fd = dxf_which_layer (nolayername, DXF_ASCII);	
    if (layer_fd == NULL)
	return(0);

    if (xflag && yflag && rflag)
    {
	arr_size = make_arc(0,centerx,centery,radius,0.0,360.0,0);
	write_polylines(layer_fd,arr_size);
    }
    return(1);
}

int 
make_arc (
    int offset, /* offset into array of points */
    double centerx,
    double centery,
    double radius,
    double start_angle,
    double finish_angle,
    int flag
)
{
    float	theta; /* the angle used for calculating a given point */
    float	alpha; /* theta converted into radians for use in math */
    double	extcirclx[4], extcircly[4];/*to check_extents of circle */
    int	x; /* looping variable */
    int arr_size;

    arr_size = offset;
    printf("making arc: offset %d  x %.1f y %.1f rad %.1f a1 %.1f a2 %.1f  %d\n", offset, centerx,centery,radius,start_angle,finish_angle,flag);
    if(start_angle > finish_angle) finish_angle = 360. + finish_angle;

    /* negative radius indicates that arc is to be drawn in a clockwise 
    ** direction from start_angle to finish_angle 
    */
    if (radius < 0) 
    {
	start_angle = 360. + start_angle;
        theta = start_angle;
	radius = -radius;
	while (theta > finish_angle)
	{
	    alpha = 3.141592654*theta/180.0; /* converting to radians */
	    xinfo[arr_size] = radius*cos(alpha) + centerx;
	    yinfo[arr_size] = radius*sin(alpha) + centery;
	    /*dxf_check_ext(pt_array[arr_size].x,pt_array[arr_size].y);*/
	    theta-= RSTEP;
	    if (arr_size == ARR_MAX)
	    {
		ARR_MAX+= ARR_INCR;
	        xinfo = (double *) G_realloc(xinfo, ARR_MAX * sizeof (double));
	        yinfo = (double *) G_realloc(yinfo, ARR_MAX * sizeof (double));
	    }
	    arr_size++;
	}
    }
    else
    {
        theta = start_angle;
	while (theta < finish_angle)/*draw arc counterclockwise */
	{
	    alpha = 3.141592654*theta/180.0; /* converting to radians */
	    xinfo[arr_size] = radius*cos(alpha) + centerx;
	    yinfo[arr_size] = radius*sin(alpha) + centery;
	    /*dxf_check_ext(pt_array[arr_size].x,pt_array[arr_size].y);*/
	    theta+= RSTEP;
	    if (arr_size == ARR_MAX)
	    {
		ARR_MAX+= ARR_INCR;
	        xinfo = (double *) G_realloc(xinfo, ARR_MAX * sizeof (double));
	        yinfo = (double *) G_realloc(yinfo, ARR_MAX * sizeof (double));
	    }
	    arr_size++;
	}
    }
    /* this insures that the last point will be correct */
    alpha = 3.141592654*finish_angle/180.0; /* converting to radians */
    xinfo[arr_size] = radius*cos(alpha) + centerx;
    yinfo[arr_size] = radius*sin(alpha) + centery;
    /*dxf_check_ext(pt_array[arr_size].x,pt_array[arr_size].y);*/
    if (arr_size == ARR_MAX)
    {
	ARR_MAX+= ARR_INCR;
	xinfo = (double *) G_realloc(xinfo, ARR_MAX * sizeof (double));
	yinfo = (double *) G_realloc(yinfo, ARR_MAX * sizeof (double));
    }
    arr_size++;

    /* if (BOUNDARIES != 4) dpg */
    {
	/*need to check extent of plotted arcs and circles */
	if (flag)  /*for an arc */
	    for(x=offset; x<arr_size; x++)
		dxf_check_ext(xinfo[x],yinfo[x]);

	else /*for a circle*/
	{
	    extcirclx[0] = centerx + radius;
	    extcircly[0] = extcircly[2] = centery;

	    extcirclx[1] = extcirclx[3] = centerx;
	    extcircly[1] = centery - radius;

	    extcirclx[2] = centerx - radius;

	    extcircly[3] = centery + radius;

	    for (x = 0; x<4; x++)
		dxf_check_ext(extcirclx[x],extcircly[x]);
	}
    }
    return arr_size - offset;
}
