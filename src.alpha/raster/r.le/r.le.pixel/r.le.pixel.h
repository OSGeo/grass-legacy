				/********************************/
				/*	  r.le.pixel.h		*/
				/*				*/
				/*	      2.1		*/
				/*				*/
				/*      07/05/94 version	*/
				/*				*/
				/*     Programmer: Baker	*/
				/*      Univ. of Wyoming	*/
				/********************************/


#include "stdio.h"
#include "math.h"
#include "ctype.h"
#include "string.h"
#include "stdlib.h"
#include "gis.h"

#define  BIG   1000000000.0
#define  MAX   800

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

struct CHOICE {
	char fn[30], reg[30], wrum;
	int  edge, tex, fb, units, z, edgemap;
	int  att[5], div[5], te2[6]; 
	int  jux[3], edg[3]; 	
};

typedef struct reglist {
        int             att;
        int             n, s, e, w;
        struct reglist  *next;
        } REGLIST;
                

/** main.c **/
void  parse_cmd();
void  parse_mv();
int   get_int();

/** driver.c **/
void  texture_fore();
void  mv_driver();
void  set_colors();
void  read_mwind();
void  meter();
void  unit_driver();
void  run_clip();
void  whole_reg_driver();
void  texture_back();
void  mail();
FILE  *fopen0();
FILE  *fopen1();
FILE  *fopen2();
FILE  *fopen3();
void  get_rich_whole();


/** cell_clip.c **/
void  cell_clip_drv();
void  cell_clip();
void  get_rich();
int   compar();


/** texture.c **/
void  mv_texture();
void  df_texture();
void  cal_att();
void  cal_divers();
void  cal_tex();
void  cal_edge();
void  read_weight();
void  read_edge();
int   find_loc();
int   find_edge();
int   check_order();

