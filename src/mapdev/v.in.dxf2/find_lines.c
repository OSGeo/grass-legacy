/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
/* last modified by J Moorman
** 7/23/90
**
** Dxf files may or may not contain a HEADER section
** if the HEADER section is found then the map extent information is
** extracted from the $EXTMAX and $EXTMIN groups
**
** in dxf_header()
** 1.if the HEADER section is not found in the file and the ENTITY
**   section is not found the program exits
** 2.if the HEADER section is found a value of 1 is returned
**   and the map extent information is searched for in this section 
**   noting that the ENTITY section must be searched for after the
**   HEADER section has been read.
**   if the map extents are successfully read in BOUNDARIES will
**   equal 4 and all calls to the check_ext() are bypassed.
** 3.if no HEADER section is found but the  ENTITY section is found 
**   a value of 0 is returned and the dxf_entities ()is bypassed
**   The map extents will be calculated at appropriate places to
**   insure that all points read in or calculated (arcs,circles) will
**   fall within the map location.
*/

#include <stdlib.h>
#include "dxf2vect.h"

int dxf_find_lines (FILE *dxf_file)
{
    int code; /* atoi of line if numeric */ 
    int header_flag; /* set to true if HEADER section found */

    /* initialize the boundary indicating variables */
    BOUNDARIES = 0;

    header_flag = dxf_header (dxf_file); /* looks for HEADER in file */
/*#ifdef JCM*/
    if (header_flag)
    {
	/* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */
	code = dxf_readcode(dxf_file); 
	while (code != 0) 
	{
	    if (code == -2)  /* EOF */
		return(0);
	    dxf_fgets (dxf_line,256,dxf_file);  
	    if (feof(dxf_file) != 0) /* EOF */
		return(0);

	    if (code == 9 && code != 0) /* only looking for 9 groups  */
	    {
		if (strcmp(dxf_line,"$EXTMAX") == 0)
		{
		    /*READS IN LINES AND PROCESSES INFORMATION UNTIL A 9 
		    ** OR A 0 IS READ IN */
		    while (code = dxf_readcode(dxf_file))
		    {
			if (code == 9)
			    break;
			if (code == -2)  /* EOF */
			    return(0);
			dxf_fgets (dxf_line,256,dxf_file);  
			if (feof(dxf_file) != 0) /* EOF */
			    return(0);
			switch (code)
			{
			    case 10:
				XMAX = atof(dxf_line);
				BOUNDARIES+=1;
				break;
			    case 20:
				YMAX = atof(dxf_line);
				BOUNDARIES+=1;
				break;
			    case 30: /*not looking for z values */
			    default:
				break;
			}
		    }
		}
		else if (strcmp(dxf_line,"$EXTMIN") == 0)
		{
		    /*READS IN LINES AND PROCESSES INFORMATION UNTIL A 9 
		    ** OR A 0 IS READ IN 
		    */

		    while (code = dxf_readcode(dxf_file))
		    {
			if (code == 9)
			    break;
			if (code == -2)  /* EOF */
			    return(0);

			dxf_fgets (dxf_line,256,dxf_file);  
			if (feof(dxf_file) != 0) /* EOF */
			    return(0);

			switch (code)
			{
			    case 10:
				XMIN= atof(dxf_line);
				BOUNDARIES+=1;
				break;
			    case 20:
				YMIN = atof(dxf_line);
				BOUNDARIES+=1;
				break;
			    case 30: /* not looking for z values */
			    default:
				break;
			}
		    }
		}
		else
		{
		    while (code = dxf_readcode(dxf_file))
		    {
			if (code == 9)
			    break;
			if (code == -2)  /* EOF */
			    return(0);
			dxf_fgets (dxf_line,256,dxf_file);  
			if (feof(dxf_file) != 0) /* EOF */
			    return(0);
		    }
		}
	    }

	    if (BOUNDARIES == 4)
	    break;

	}
	/* SHOULD CHECK FOR THE RETURN VALUE OF THIS FUNCTION */ 
	dxf_entities (dxf_file);
	/* looks for ENTITIES in file */ 
    } 
/*#endif*/
    xinfo = (double *) G_malloc (ARR_INCR * sizeof (double));
    yinfo = (double *) G_malloc (ARR_INCR * sizeof (double));
    ARR_MAX = ARR_INCR;
    n = e = DBL_MIN;
    w = s = DBL_MAX;
    dxf_fgets (dxf_line, 256, dxf_file);
    while (feof (dxf_file) == 0)
    {
	if (strcmp (dxf_line, polyline) == 0)
	    dxf_add_polyline (dxf_file);

	else if (strcmp (dxf_line, line) == 0)
	    dxf_add_line (dxf_file);

	else if (strcmp (dxf_line, point) == 0)
	    dxf_add_point (dxf_file);

	else if (strcmp (dxf_line, arc) == 0)
	    dxf_add_arc (dxf_file);

	else if (strcmp (dxf_line, circle) == 0)
	    dxf_add_circle (dxf_file);

	else if (strcmp (dxf_line, text) == 0)
  	       dxf_add_labelbox (dxf_file);

	dxf_fgets (dxf_line, 256, dxf_file);
    }
    return 0;
}
