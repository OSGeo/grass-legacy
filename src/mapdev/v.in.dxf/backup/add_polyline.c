/* written by J Moorman
** 7/23/90
*/

#include "dig_defines.h"
#include "dig_head.h"
#include "dxf2vect.h"

/* DECLARING SUBROUTINES */
double 	atof();


dxf_add_polyline (dxf_file)
FILE	*dxf_file;
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
    char *nolayername = "UNIDENTIFIED"; 
    char layername[80];
    DXF_DIG *layer_fd;         /* POINTER TO LAYER NAME */
    int  code;  /* VARIABLE THAT HOLDS VALUE RETURNED BY readcode() */

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */

    while ((code = dxf_readcode(dxf_file)) != 0) 
    {
	if (code == -2)  /* EOF */
	return(0);
	dxf_fgets (dxf_line,80,dxf_file);  
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
		/* NOTE: CODE ONLY EXISTS FOR FLAG = 1 or 0 (CLOSED POLYLINE) */
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
	    default:
		break;
	}
    }
    dxf_fgets (dxf_line,80,dxf_file);  
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
		dxf_fgets (dxf_line,80,dxf_file);  
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
		    case 30: /* Z COORDINATE NOT BEING USED */
		    /* NOTE: THERE ARE MORE CASES POSSIBLE */
		    default:
			break;
		}
	    }
	}
	if(xflag == 1  && yflag ==1)
	{
	    /* if (BOUNDARIES != 4) dpg *//*if map extents not read in from dxf file */
	    dxf_check_ext (xinfo[arr_size],yinfo[arr_size]);
	    if ((arr_size) >= ARR_MAX-1)
	    {
		ARR_MAX += ARR_INCR;
		xinfo = (double *) G_realloc(xinfo, ARR_MAX * sizeof (double));
		yinfo = (double *) G_realloc(yinfo, ARR_MAX * sizeof (double));
	    }
	    arr_size ++;
	}
	dxf_fgets (dxf_line,80,dxf_file);  
    }
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
write_polylines(layer_fd,arr_size)
    DXF_DIG	*layer_fd;
	int		arr_size;
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
	fprintf (layer_fd->fd, " %12.2lf %12.2lf \n", 
		 yinfo[count], xinfo[count]);
    }
}
