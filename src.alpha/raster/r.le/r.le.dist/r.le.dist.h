				/********************************/
				/*	  r.le.dist.h		*/
				/*				*/
                                /*              2.1             */
                                /*                              */
                                /*       07/15/94 version       */
				/*				*/
				/*       Programmer: Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/

#include "stdio.h"
#include "setjmp.h"
#include "math.h"
#include "ctype.h"
#include "stdlib.h"
#include "string.h"
#include "gis.h"

#define  BIG  10000000
#define  MIN  5
#define  K1   50
#define  K2   200
#define  K3   500
#define  K4   2000
#define  NULLPTR  (PATCH *) 0

typedef struct __dirdesc {
        int     dd_fd;          /* file descriptor */
        long    dd_loc;         /* buf offset of entry from last readddir() */
        long    dd_size;        /* amount of valid data in buffer */
        long    dd_bsize;       /* amount of entries read at a time */
        long    dd_off;         /* Current offset in dir (for telldir) */
        char    *dd_buf;        /* directory data buffer */
} DIR;

extern  DIR *opendir(/* char *dirname */);
extern  struct dirent *readdir(/* DIR *dirp */);
extern  int closedir(/* DIR *dirp */);

typedef struct pt {
        int          row, col;
        struct pt    *next;
        } PT;

typedef struct patch {
	int          att, num;
	double	     c_row, c_col, n, s, e, w, npts;
	int          *col, *row;
	struct patch *next;
	} PATCH;


struct CHOICE {
	char fn[30], reg[30], out[30],
	     wrum;
	int  fb, trace, patchmap, units;
	int  mm[6], mn[10];	
	};


struct NB{
	int   num;
	float d;
	int   r;
	int   c;
       };



struct NPL{
	int     d;
	PATCH   *p;
	};


struct VALUE{
	double sum, sum2;
	double sum1[25], sum21[25];
	int    den[25], den2[25][25];
	};

typedef struct reglist {
        int             att;
        int             n, s, e, w;
        struct reglist  *next;
        } REGLIST;


/** main.c **/
void  user_input();

/** driver.c **/
void  dist_fore();
void  open_files();
void  read_line();
void  read_recl_tb();
void  read_para();
void  get_para();
void  free_para();

void  mv_driver();
void  run_dist_mv();
void  do_calc_mv();
void  read_mwind();
void  set_colors();
void  set_choice();

void  whole_reg_driver();
void  unit_driver();
void  run_clip();

void  dist_back();
void  mail();
void  meter();
FILE  *fopen0();


/** trace.c **/
void  	cell_clip_drv();
int 	is_not_empty_buffer();
int  	center_is_not_zero();
void  	cell_clip();
void  	trace();
PATCH 	*get_bd();
int   	yes_nb();
void 	clockwise();

/** dist.c **/
void    df_dist();
void    setup_v();
double  eu_dist();
double  eu_d();
int     in_group();
int     check_order();
int     index_coh();
int     recl_coh();

void    mv_dist();
void    run_dist();

void    cal_adj();
void    srch_bdry();
PATCH   *get_patch();
void    set_nb_list();
void    check_nbs();

void    save_dist();
void    save_dist1();
void    save_dist2();

void    cal_nrst();
void    do_calc();
int     skip_int();
void    srch_nrst();
int     cal_box_d();
void    cal_dist();

