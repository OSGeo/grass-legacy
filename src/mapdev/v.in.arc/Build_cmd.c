
/* Build_cmd.c  (cmd-line version of BuildDig.c--dks)
 *
 * function defined:
 *
 * BuildDig(cov_type, neatline,
 *          lines_file, pts_file, txt_file,
 *          dig_file, atts_file, cats_filename, idcol, catcol, attcol);
 * 
 *    int cov_type;        - either "line" or "polygon" (was char*--dks)
 *    int  neatline;       - true if neatline is desired 
 *    FILE *lines_file,    - ARC/INFO generate format lines file
 *         *pts_file,      - ARC/INFO generate format point-labels
 *         *txt_file,      - file assoc. text with attrib. numbers
 *         *dig_file,      - GRASS vector (dig) file to be created
 *         *atts_file;     - GRASS vector attribute (dig_atts) file 
 *                           to be created
 *    char *cats_filename; - GRASS vector category (dig_cats) file
 *                           to be created
 *
 * PURPOSE: builds a GRASS vector file from ARC/INFO Generate
 * files that represent either an line or polygon coverage.
 *
 * NOTES: none
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#include <stdio.h>
#include "Vect.h"
#include "AtoG.h"
#include "v_in_arc.cmd.h"

int BuildDig (int cov_type, int neatline,
    FILE *lines_file, FILE *pts_file, FILE *txt_file, FILE *atts_file,
    struct Map_info *VectMap, char *cats_filename,
    int idcol, int catcol, int attcol)
{
#ifdef DEBUG 
fprintf (stdout,"BuildDig\n");
#endif

if (cov_type == LINE_TYPE)
   {
   if (txt_file == NULL)
      {
      if (GenToDigLine(lines_file,VectMap,0,cats_filename) < 0)
         return(-1);
	  else {
		  return (1);
		  }
      }
   else 
      {
      if (GenToLabelledDigLines(lines_file,txt_file,
         atts_file,VectMap, cats_filename, idcol, catcol, attcol)<0)
         return(-2);
	  else {
		  return (1);
		  }
      }
   }
else if (cov_type == POLY_TYPE)
   {
   if (pts_file != NULL && txt_file!=NULL)
      {
      if (GenToDigAreaLabels(pts_file,txt_file,atts_file,cats_filename,
			idcol, catcol, attcol)<0)
         return(-3);
      } 
   if (GenToDigArea(lines_file,VectMap,neatline,cats_filename)<0)
   {
      return(-4);
   }
   else
	  return (1);
   }
return(-5);
}
