                        /********************************/
                        /*     r.le.setup/setup.h   	*/
                        /*                              */
                        /*             2.1              */
                        /*                              */
                        /*       07/25/94 version       */
                        /*                              */
                        /*      Programmer: Baker       */
                        /*      Univ. of Wyoming        */
                        /********************************/

#include "stdio.h"
#include "math.h"
#include "signal.h"
#include "setjmp.h"
#include "ctype.h"
#include "stdlib.h"
#include "string.h"
#include "gis.h"
#include "sys/types.h"


/* #include "dig_defines.h" */
/* #include "dig_structs.h" */
/* #include "dig_head.h" */



#define  SML   0.5
#define EQ(a, b)    (a-b < 0.01 && a-b > -0.01 )
#define BIG   1000000000.0
jmp_buf jmp;

struct  dirent {
	off_t           d_off;          /* offset of next disk dir entry */
	unsigned long   d_fileno;       /* file number of entry */
	unsigned short  d_reclen;       /* length of this record */
	unsigned short  d_namlen;       /* length of string in d_name */
	char            d_name[255+1];  /* name (up to MAXNAMLEN + 1) */
};


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

/** main.c **/

void  get_pwd();

/** sample.c **/
void  sample();
void  man_unit();
void  draw_grid();
int   calc_unit_loc();
void  get_rd();
void  f();
int   overlap();
int   calc_num();
void  graph_unit();
void  draw_box();
void  numtrap();


/** mv_wind.c
void  mov_wind();


/** ask_group.c **/
void  backup_file();
void  ask_group();
void  get_group_drv();
void  ask_limits();
void  ask_move_recl();
void  get_index();
void  ask_reclass();
void  get_1recl();
void  ask_fromto();
int   search_fn();
FILE  *fopen0();


/** setup.c **/
void  set_map();
void  change_color();
void  set_frame();
void  set_rgn();
void  def_rgn();
void  ppoint();
void  pbutton();
void  save_rgn();
void  print_hd();
void  scr_cell();
void  paint_map();


