/* written by J Moorman
** 7/23/90
*/

#include "dxf2vect.h"

double atof();


dxf_add_point (dxf_file)
FILE	*dxf_file;
{
    /* DECLARING VARIABLES */
    int  layer_flag = 0;    /* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int  xflag = 0;         /* INDICATES IF A x VALUE HAS BEEN FOUND */
    int  yflag = 0;         /* INDICATES IF A y value has been found */
    char *nolayername = "UNIDENTIFIED"; 
    FILE *layer_fd;         /* POINTER TO LAYER NAME */
    FILE *dxf_which_layer();   /* ASSIGNS FILE POINTER TO LAYER NAME */ 
    int  code;  /* VARIABLE THAT HOLDS VALUE RETURNED BY readcode() */
    int  count; /* LOOPING VARIABLE */ 

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */

    while ((code = dxf_readcode(dxf_file)) != 0) {
	if (code == -2)  /* EOF */
	    return(0);
	dxf_fgets (dxf_line,80,dxf_file);  
	if (feof(dxf_file) != 0) /* EOF */
	    return(0);
	
	switch (code) {
	    case   8:
		if(!layer_flag) {
		    layer_fd = dxf_which_layer (dxf_line, DXF_ASCII);	
		    if (layer_fd == NULL)
		       return(0); 
		    layer_flag = 1;
		}
		break;
	    case 10: /* x COORDINATE */
		pt_array[arr_size].x  = atof(dxf_line);	
		xflag = 1;
		break;
	    case 20: /* y COORDINATE */
		pt_array[arr_size].y = atof(dxf_line);	
		yflag = 1;
		break;
	    case 30: /* Z COORDINATE NOT BEING USED */
	    case 50: /* ANGLE OF x AXIS FOR THE UCS IN EFFECT */

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

    if(xflag == 1  && yflag ==1){
	if (BOUNDARIES != 4)/* if map extents not read in from dxf file */
	    dxf_check_ext (pt_array[arr_size].x,pt_array[arr_size].y);

	if (!layer_flag){ /* NO LAYER DESIGNATED */
	    layer_fd = dxf_which_layer (nolayername, DXF_ASCII);
	if (layer_fd == NULL)
	    return(0);
	}
	fprintf (layer_fd, "P  2\n");
	for (count = 0; count < 2 ; count++) 
	    fprintf (layer_fd, " %12.2lf %12.2lf \n", 
		     pt_array[0].y, pt_array[0].x);
    }
    return(1);
}
