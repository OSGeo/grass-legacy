/*-
 * $Log$
 * Revision 1.4  2000-01-02 12:23:20  markus
 * again comments fixed
 *
 * Revision 1.3  2000/01/02 12:21:47  markus
 * comments fixed
 *
 * Revision 1.2  2000/01/02 12:18:16  markus
 * Bill Hughes: some variable types fixed
 *
 * Revision 1.1.1.1  1999/12/29 15:10:23  markus
 * initial CVS import
 *
 * Revision 1.13  1996/05/23  brown - brown@gomez.gis.uiuc.edu
 * changed DateTime stuff to use TimeStamp instead
 *
 * Revision 1.12  1995/07/17  11:16:26  mccauley
 * took out has_cat and made part of struct
 * support for floating point categories
 *
 * Revision 1.11  1995/06/20  10:26:48  mccauley
 * moved prototypes to external file: P_site.h
 *
 * Revision 1.10  1995/05/24  00:05:58  mccauley
 * added DateTime stuff
 *
 * Revision 1.9  1995/04/17  22:58:24  mccauley
 * added "typedef struct ... Site_head;"
 *
 * Revision 1.8  1995/02/22  02:46:20  mccauley
 * added sites functions from 4.1 that we'll keep.
 *
 * Revision 1.7  1995/02/22  02:25:28  mccauley
 * changed names of functions to G_site_xxx().
 *
 * Revision 1.6  1995/02/22  02:16:41  mccauley
 * increased MAX_SITE_LEN and MAX_SITE_STRING on suggestion
 * of Michael Shapiro <mshapiro@ncsa.uiuc.edu>.
 *
 * Revision 1.5  1995/02/21  07:56:11  mccauley
 * added pragma ident
 *
 * Revision 1.4  1995/02/21  07:28:18  mccauley
 * added qsort comparison function definitions.
 *
 * Revision 1.3  1995/02/08  23:10:46  mccauley
 * added prototype for G_guess_site_fmt.
 *
 * Revision 1.2  1995/02/07  23:18:00  mccauley
 * added prototypes for G_new_get_site and G_new_put_site
 *
 * Revision 1.1  1995/02/07  21:00:51  mccauley
 * Initial revision
 * 
 */

/*-
 * easting|northing|[z|[d4|]...][#category] [ [@attr_text OR %flt] ... ]
 *
 * to allow multidimensions (everything preceding the last '|') and any
 * number of text or numeric attribute fields.
 */

#define MAX_SITE_STRING 1024
#define MAX_SITE_LEN 4096
 
typedef struct
{
  double east, north;
  double *dim;
  int dim_alloc;
  RASTER_MAP_TYPE cattype;
  CELL ccat;
  FCELL fcat;
  DCELL dcat;
  int str_alloc;
  char **str_att;
  int dbl_alloc;
  double *dbl_att;
} Site;
 
typedef struct
{
  char *name, *desc, *form, *labels, *stime;
  struct TimeStamp *time;
} Site_head;

#include "P_site.h"

