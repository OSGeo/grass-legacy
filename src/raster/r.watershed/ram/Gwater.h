/* program to map out drainage basin structure  */
/* this one uses the A * search algorithm   	*/
/* written by Chuck Ehlschlaeger        	*/
/* last modified 03/26/91           		*/
#include <math.h>
#include "gis.h"
#include "ramseg.h"
#include "flag.h"

/* redefining G_malloc allows you to see where in
program that memory runs out
#define G_malloc malloc
*/

#define AR_SIZE			16
#define AR_INCR			16
#define SHORT			int
#define NOMASK			1
#define MIN_SLOPE		.00001
#define MIN_GRADIENT_DEGREES	1
#define DEG_TO_RAD		.017453293  /* pi / 180 */
#define METER_TO_FOOT		3.281
#define MAX_BYTES		2000000
#define PAGE_BLOCK		512
#define RITE			1
#define LEFT			2
#define NEITHER			0
#define ABS(x)	(((x) < 0) ? -(x) : (x))
#define TSTSTR(a)	(fprintf (stderr, "%s\n", a))
#define TST(a)		(fprintf (stderr, "%e\n", (double) (a)))

#define POINT       struct points
POINT   
{
    SHORT       r, c, downr, downc;
    int		nxt;
};

#ifdef MAIN
#define GLOBAL
#define DRAINVAR	= {{ 7,6,5 },{ 8,0,4 },{ 1,2,3 }}
#define UPDRAINVAR	= {{ 3,2,1 },{ 4,0,8 },{ 5,6,7 }}
#define NEXTDRVAR	= { 1,-1,0,0,-1,1,1,-1 }
#define NEXTDCVAR	= { 0,0,-1,1,1,-1,1,-1 }
#else
#define GLOBAL extern
#define DRAINVAR
#define UPDRAINVAR
#define NEXTDRVAR
#define NEXTDCVAR
#endif

GLOBAL struct Cell_head		window;

GLOBAL int	first_astar, first_cum, nxt_avail_pt, total_cells, do_points;
GLOBAL SHORT    nrows, ncols;
GLOBAL double   half_res, diag, max_length, dep_slope;
GLOBAL int	bas_thres, tot_parts;
GLOBAL FLAG     *worked, *in_list, *s_b, *swale;
GLOBAL RAMSEG   dis_seg, alt_seg, wat_seg, asp_seg, bas_seg, haf_seg;
GLOBAL RAMSEG	r_h_seg, dep_seg;
GLOBAL RAMSEG   slp_seg, s_l_seg, s_g_seg, l_s_seg;
GLOBAL POINT	*astar_pts;
GLOBAL CELL     *dis, *alt, *wat, *asp, *bas, *haf, *r_h, *dep;
GLOBAL CELL	*ril_buf;
GLOBAL int	ril_fd;
GLOBAL double   *s_l, *s_g, *l_s;
GLOBAL CELL	one, zero;
GLOBAL double	ril_value, dzero;
GLOBAL SHORT    sides;
GLOBAL SHORT	drain[3][3]	DRAINVAR; 
GLOBAL SHORT	updrain[3][3]	UPDRAINVAR; 
GLOBAL SHORT	nextdr[8]	NEXTDRVAR;
GLOBAL SHORT	nextdc[8]	NEXTDCVAR;
GLOBAL char     ele_name[40], *ele_mapset, pit_name[40], *pit_mapset;
GLOBAL char     run_name[40], *run_mapset, ob_name[40], *ob_mapset;
GLOBAL char	ril_name[40], *ril_mapset, dep_name[40], *dep_mapset;
GLOBAL char	*this_mapset;
GLOBAL char     seg_name[40], bas_name[40], haf_name[40], thr_name[8];
GLOBAL char     ls_name[40], st_name[40], sl_name[40], sg_name[40];
GLOBAL char     wat_name[40], asp_name[40], arm_name[40], dis_name[40];
GLOBAL char     ele_flag, pit_flag, run_flag, dis_flag, ob_flag;
GLOBAL char     wat_flag, asp_flag, arm_flag, ril_flag, dep_flag;
GLOBAL char     bas_flag, seg_flag, haf_flag, er_flag;
GLOBAL char	st_flag, sb_flag, sg_flag, sl_flag, ls_flag;
GLOBAL FILE     *fp;
