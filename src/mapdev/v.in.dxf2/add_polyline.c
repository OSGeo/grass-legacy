/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char layername */
/* written by J Moorman
** 7/23/90
*/

#include <stdlib.h>
#include <math.h>
#include "Vect.h"
#include "dxf2vect.h"

#define DEG_TO_RAD (3.141592654/180.0)

int dxf_add_polyline (FILE *dxf_file)
{
    /* DECLARING VARIABLES */
    int  layer_flag = 0;    /* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int  vert_flag;         /* INDICATES THAT VERTICES ARE FOLLOWING */
    int  polyline_flag = 0; /* INDICATES THE TYPE OF POLYLINE */
    int  xflag = 0;         /* INDICATES IF A x VALUE HAS BEEN FOUND */
    int  yflag = 0;         /* INDICATES IF A y VALUE HAS BEEN FOUND */
    int  nu_layer_flag = 1; /* INDICATES IF A nu_layer WAS FOUND */ 	
    int  fprintf_flag1 = 1; /* INDICATES IF ERROR MESSAGE PRINTED ONCE */
    int  fprintf_flag2 = 1; /* INDICATES IF ERROR MESSAGE PRINTED ONCE */
    int arr_size = 0;
    int arc_arr_size = 0;
    double x1, x2, y1, y2, cent_y, cent_x, rad, beta, half_alpha;
    float ang1, ang2;
    /* variables to create arcs */
    double bulge = 0.0;           /* for arc curves */
    double prev_bulge = 0.0;           /* for arc curves */
    double arc_tan = 0.0;           /* for arc curves */
    char *nolayername = "UNIDENTIFIED"; 
    char layername[256];
    DXF_DIG *layer_fd;         /* POINTER TO LAYER NAME */
    int  code;  /* VARIABLE THAT HOLDS VALUE RETURNED BY readcode() */

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
		    strcpy(layername,dxf_line);
		    layer_flag = 1;
		}
		break;
	    case  66: /* FLAG BIT VALUE MEANING VERTICES FOLLOW FLAG */ 
		vert_flag = atoi(dxf_line);
		if (vert_flag != 1) /* flag must always be 1 */
		    if (fprintf_flag1)
		    {
			fprintf(stderr,"TEXT: vertices following flag missing");
			fprintf_flag1 = 0;
		    }
		/* NOTE: WARNING PRINTED ONLY */
		break;
	    case  70: /* POLYLINE FLAGS */
		polyline_flag = atoi(dxf_line);

		/* polyline flag is 1 for closed polyline
				    2 curve fit vertices have been added
				    4 spline fit vertices have been added
                */
		/* NOTE: CODE ONLY EXISTS FOR FLAG = 1 (CLOSED POLYLINE) or 0 */
		if (polyline_flag&8 || polyline_flag & 16 || polyline_flag & 32)
		    if (fprintf_flag2)
		    {
			fprintf(stderr,"WARNING: 3-d data in dxf file\n");
			fprintf_flag2 = 0;
		    }
	    break;

	    /* THE FOLLOWING GROUPS USED ONLY IF DIFFERENT THAN DEFAULTS */		   
	    case   6: /* LINETYPE NAME */
	    case  38: /* ELEVATION IF NONZERO */
	    case  39: /* THICKNESS IF NONZERO */
	    case  62: /* COLOR NUMBER (IF NOT "BYLAYER") */
	    case 210: /* X EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	    case 220: /* Y EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	    case 230: /* Z EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */

	    /* THE FOLLOWING GROUPS ARE SPECIFIC TO POLYLINE ENTITY */
	    case  41: /* DEFAULT ENDING WIDTH */
	    case  71: /* POLYGON MESH */
	    case  72: /* POLYGON MESH */
	    case  75: /* SMOOTH SURFACE TYPE -OPTIONAL */ 
		      /* not used */
	    default:  
		break;
	}
    }
    dxf_fgets (dxf_line,256,dxf_file);  
    while (strcmp (dxf_line,seqend) != 0)/* LOOP UNTIL SEQEND IN THE DXF FILE */
    {
	if (feof(dxf_file) != 0) /* EOF */
	return(0);
	if (strcmp (dxf_line,vertex) == 0) 
	{
	    xflag = 0;
	    yflag = 0;
	    while ((code = dxf_readcode (dxf_file)) != 0) 
	    {
		if (code == -2) /* EOF */
		return (0);
		dxf_fgets (dxf_line,256,dxf_file);  
		if (feof(dxf_file) != 0) /* EOF */
		return(0);
		switch (code)
		{
		    case  8: /* LAYER NAMES ARE INCLUDED IN VERTEX ENTITY */
			if(!layer_flag){ /* IF NO LAYER PREVIOUSLY ASSIGNED */
			    layer_fd =dxf_which_layer (dxf_line, DXF_ASCII);
			    if (layer_fd == NULL)
				return(0);
			    strcpy(layername,dxf_line);
			    layer_flag = 1;
			}
			else /* COMPARING layer_fd IN POLYLINE ENTITY */
			    layer_fd = dxf_which_layer(dxf_line,DXF_ASCII);
			if (layer_fd == NULL)
			    return(0);
			if ((strcmp(dxf_line,layername)) != 0 && 
			    nu_layer_flag ==1) 
			{
			    fprintf(stderr,
				"ERROR: layer name %s listed but not used \n",
				    dxf_line);
			    nu_layer_flag = 0; /* so ERROR only printed once */
			}
			break;
		    case 10: /* x COORDINATE */
			xinfo[arr_size]  = atof(dxf_line);	
			xflag = 1;
			break;
		    case 20: /* y COORDINATE */
			yinfo[arr_size] = atof(dxf_line);	
			yflag = 1;
			break;
		    case 30: break;/* Z COORDINATE NOT BEING USED */
		    case 42: /* bulge */
			     bulge = atof(dxf_line);
			     break;
		    case 50: /* curve fit tangent */
		    case 70: /* vertex flags */
			     if( atoi(dxf_line) == 16)
			     {
				/* spline frame control point: don't draw it! */
				xflag = 0;
				yflag = 0;
                             }
			     break;
		    /* NOTE: THERE ARE MORE CASES POSSIBLE */
		    default:
			break;
		}
	    }
	}
	if(xflag == 1  && yflag ==1)
	{
	    /* if prev segment is an arc  (prev_bulge != 0) prepare to make arc */
	    if(prev_bulge > 0.0)
	          arc_tan = prev_bulge;
            else if(prev_bulge < 0.0)
		  arc_tan = (-1.0) * prev_bulge;

	    if(arc_tan == 0.0) /* straight line segment */
	    {
	         dxf_check_ext (xinfo[arr_size],yinfo[arr_size]);
	         if ((arr_size) >= ARR_MAX-1)
	         {
		     ARR_MAX += ARR_INCR;
		     xinfo = (double *) G_realloc(xinfo, ARR_MAX * sizeof (double));
		     yinfo = (double *) G_realloc(yinfo, ARR_MAX * sizeof (double));
	         }
	         arr_size ++;
            }
	    else if(!(xinfo[arr_size-1] == xinfo[arr_size] && yinfo[arr_size-1] == yinfo[arr_size]))
	    /* make an arc */
	    {
		/* compute cent_x, cent_y, ang1, ang2 */
                if(prev_bulge > 0.0)
		{
		   x1 = xinfo[arr_size-1];
		   x2 = xinfo[arr_size];
		   y1 = yinfo[arr_size-1];
		   y2 = yinfo[arr_size];
		}
		else
		{
		/* figure out how to compute the opposite center */
		   x2 = xinfo[arr_size-1];
		   x1 = xinfo[arr_size];
		   y2 = yinfo[arr_size-1];
		   y1 = yinfo[arr_size];
		}
		half_alpha = (double) atan(arc_tan) * 2.;
		rad = hypot(x1 - x2, y1 - y2) * .5 / sin(half_alpha);
	        beta = atan2(x1 - x2, y1 - y2);
		/* now bring it into range 0 to 360 */
		beta = 90.0 * DEG_TO_RAD - beta;
		if(beta <= 0.0 ) beta = 360.0 * DEG_TO_RAD + beta;
		/* now beta is counter clock wise from 0 (direction of (1,0)) to 360 */
                if(beta >= 0.0 && beta < 90.0)
		{
		   cent_x = x2 + rad *  sin(half_alpha + beta);
		   cent_y = y2 - rad *  cos(half_alpha + beta);
		   ang2 = (half_alpha + beta) /  DEG_TO_RAD + 90.0;
		   ang1 = (beta - half_alpha) /  DEG_TO_RAD + 90.0;
                }
		else if(beta >= 90.0 && beta < 180.0)
		{
		   beta -= 90.0;
		   cent_y = y2 + rad *  sin(half_alpha + beta);
		   cent_x = x2 + rad *  cos(half_alpha + beta);
		   ang2 = (half_alpha + beta) / DEG_TO_RAD + 180.0;
		   ang1 = (beta - half_alpha) / DEG_TO_RAD + 180.0;
                }
		else if(beta >= 180.0 && beta < 270.0)
		{
		   beta -= 180.0;
		   cent_x = x2 - rad *  sin(half_alpha + beta);
		   cent_y = y2 + rad *  cos(half_alpha + beta);
		   ang2 = (half_alpha + beta) / DEG_TO_RAD + 270.0;
		   ang1 = (beta - half_alpha) / DEG_TO_RAD + 270.0;
                }
		else /* 270 <= beta < 360 */
		{
		   beta -= 270.0;
		   cent_y = y2 - rad * sin(half_alpha + beta);
		   cent_x = x2 - rad *  cos(half_alpha + beta);
		   ang2 = (half_alpha + beta) / DEG_TO_RAD;
		   ang1 = (beta - half_alpha) / DEG_TO_RAD;
                }

	        arr_size --; /* disregard last 2 points */
		if(prev_bulge < 0.0) 
		       arc_arr_size = make_arc(arr_size,cent_x,cent_y,
				 -rad, ang2, ang1,1);
                       /* arc is going in clockwise direction from x2 to x1 */
                else

		       arc_arr_size = make_arc(arr_size,cent_x,cent_y,
				 rad,ang1,ang2,1);
	        arr_size += arc_arr_size;
	        while ((arr_size) >= ARR_MAX)
	        {
	    	   ARR_MAX += ARR_INCR;
	    	   xinfo = (double *) G_realloc(xinfo, ARR_MAX * sizeof (double));
		   yinfo = (double *) G_realloc(yinfo, ARR_MAX * sizeof (double));
	        }
	    } /* arc */
  	    prev_bulge = bulge;
	    arc_tan = 0.0;
	    bulge = 0.0;
	} /* processing polyline vertex */
	dxf_fgets (dxf_line,256,dxf_file);  
    } /* vertex loop */
    /* done reading vertices */
    if (polyline_flag & POLYFLAG1) /* ONLY DEALING WITH polyline_flag = 1 */
    {
	/* CHECK TO MAKE SURE VERTEX POINTS DESCRIBE A CLOSED POLYLINE */
	if (xinfo[0] != xinfo[arr_size-1] || yinfo[0] != yinfo[arr_size-1])
	{
	    /* ADD ON THE VERTEX POINT TO COMPLETE CLOSED POLYLINE */
	    xinfo[arr_size] = xinfo[0];
	    yinfo[arr_size] = yinfo[0];

	    /* arr_size INCREMENTED TO BE CONSISTENT WITH POLYLINE_FLAG != 1 */
	    if ((arr_size) >= ARR_MAX-1)
	    {
		ARR_MAX+= ARR_INCR;
		xinfo = (double *) G_realloc (xinfo, ARR_MAX * sizeof (double));
		yinfo = (double *) G_realloc (yinfo, ARR_MAX * sizeof (double));
	    }
	    arr_size ++; 
	}
    }
    if (!layer_flag)
    { /* NO LAYER DESIGNATED */
	layer_fd = dxf_which_layer (nolayername, DXF_ASCII);
	if (layer_fd == NULL)
	    return(0);
    }
    write_polylines(layer_fd,arr_size);
    return(1);
}

/* PRINTS OUT THE POLYLINE VERTEX DATA TO FILE DESIGNATED AS layer_fd */
int 
write_polylines (DXF_DIG *layer_fd, int arr_size)
{
    int  count; /* LOOPING VARIABLE */


    if(!ascii_flag->answer) /* FOR USE IN BINARY FILE */
    {
	Vect_copy_xy_to_pnts (Points, xinfo, yinfo, arr_size);
	Vect_write_line(layer_fd->Map,LINE,Points);
    }
    else
    {
	fprintf (layer_fd->fd, "L  %d\n", arr_size);
	for (count = 0; count < arr_size ; count++) 
	fprintf (layer_fd->fd, " %12.2f %12.2f \n", 
		 yinfo[count], xinfo[count]);
    }

    return 0;
}
