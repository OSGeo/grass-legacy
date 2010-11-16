/* These next defines determine the size of the sub-window that will
 * be held in memory.  Larger values will require
 * more memory (but less i/o) If you increase these values, keep  in
 * mind that although you think the i/o will decrease, system paging
 * (which goes on behind the scenes) may actual increase the i/o.
 */

#include <grass/imagery.h>
#include <grass/ortholib.h>
#include <grass/glocale.h>
#include "defs.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

/* activate debug in Gmakefile */
#ifdef  DEBUG3
GLOBAL FILE *Bugsr;
#endif

#define L2BDIM 6
#define BDIM (1<<(L2BDIM))
#define L2BSIZE (2*(L2BDIM))
#define BSIZE (1<<(L2BSIZE))
#define HI(i) ((i)>>(L2BDIM))
#define LO(i) ((i)&((BDIM)-1))

typedef DCELL block[BDIM][BDIM];

struct cache
{
    int fd;
    int stride;
    int nblocks;
    block **grid;
    block *blocks;
    int *refs;
};

typedef void (*func) (struct cache *, void *, int, double *, double *, struct Cell_head *);

GLOBAL func interpolate;		/* interpolation routine        */

GLOBAL int seg_mb_img, seg_mb_elev;
GLOBAL char *method;
GLOBAL int temp_fd;
GLOBAL CELL **cell_buf;
GLOBAL char *temp_name;

GLOBAL int *ref_list;
GLOBAL char **new_name;


GLOBAL struct Ortho_Image_Group group;
GLOBAL struct Ortho_Photo_Points cp;
GLOBAL struct Ortho_Control_Points cpz;
GLOBAL struct Ortho_Control_Points temp_points;
GLOBAL struct Ortho_Camera_File_Ref cam_info;

GLOBAL struct Cell_head elevhd;
GLOBAL char *elev_layer;
GLOBAL char *mapset_elev;


/* georef coefficients */
GLOBAL double E12[3], N12[3], Z12[3];
GLOBAL double E21[3], N21[3], Z21[3];
GLOBAL double E12a, E12b, E12c, N12a, N12b, N12c;
GLOBAL double E21a, E21b, E21c, N21a, N21b, N21c;

GLOBAL struct Cell_head target_window;

#include "local_proto.h"
