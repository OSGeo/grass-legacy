/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
/* written by J Moorman
** 7/23/90
*/

#include <stdlib.h>
#include "dxf2vect.h"
#include "Vect.h"

int dxf_add_point (FILE *dxf_file)
{
    /* DECLARING VARIABLES */
    int  layer_flag = 0;    /* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int  xflag = 0;         /* INDICATES IF A x VALUE HAS BEEN FOUND */
    int  yflag = 0;         /* INDICATES IF A y value has been found */
    char *nolayername = "UNIDENTIFIED"; 
    DXF_DIG *layer_fd;         /* POINTER TO LAYER NAME */
    int  code;  /* VARIABLE THAT HOLDS VALUE RETURNED BY readcode() */
    int  count; /* LOOPING VARIABLE */ 

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
		xinfo[0] = atof(dxf_line);	
		xflag = 1;
		break;
	    case 20: /* y COORDINATE */
		yinfo[0] = atof(dxf_line);	
		yflag = 1;
		break;
#ifdef FOO
	    case 30: /* Z COORDINATE NOT BEING USED */
	    case 50: /* ANGLE OF x AXIS FOR THE UCS IN EFFECT */

	    /* THE FOLLOWING GROUPS USED ONLY IF DIFFERENT THAN DEFAULTS */		   
	    case  38: /* ELEVATION IF NONZERO */
	    case  39: /* THICKNESS IF NONZERO */
	    case  62: /* COLOR NUMBER (IF NOT "BYLAYER") */
	    case 210: /* X EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	    case 220: /* Y EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	    case 230: /* Z EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
#endif

	    default:
		break;
	}
	if(xflag == 1  && yflag ==1)
	{
	    dxf_check_ext (xinfo[0],yinfo[0]);
	    xflag = 0;
	    yflag = 0;
	    if (!layer_flag)
	    { /* NO LAYER DESIGNATED */
		layer_fd = dxf_which_layer (nolayername, DXF_ASCII);
		if (layer_fd == NULL)
		    return(0);
	    }
	    /* PRINTS OUT THE POLYLINE VERTEX DATA TO FILE DESIGNATED AS layer_fd */
	    xinfo[1] = xinfo[0];
	    yinfo[1] = yinfo[1];
	    /* FOR BINARY FILES */
	    if(!ascii_flag->answer)
	    {
		Vect_copy_xy_to_pnts (Points, xinfo, yinfo, 2);
		Vect_write_line(layer_fd->Map,DOT,Points);
	    }
	    else
	    {
		fprintf (layer_fd->fd, "P  %d\n",2);
		for (count = 0; count < 2 ; count++) 
		    fprintf (layer_fd->fd, " %12.2f %12.2f \n", yinfo[count],
			xinfo[count]);
	    }
	}
    }
    return(1);
}
