#include "gis.h"
/* #define MAXFILES    30  Original setting */

#define MAXFILES    1024
/*  We made the above change to fix the "Too many open files" problem.  
* The problem is that MAXFILES was used to compare with fd (file descriptor) 
* which is NOT a counter of open files in a process.  fd is assigned by the 
* system and it can be greater than MAXFILES even if the actual number of 
* open files is less than MAXFILES.  Since UNIX has a system level variable 
* OPEN_MAX (default = 64 for Solaris, 256 for some LINUX) it is safe to let 
* the system to control the number of open files.  By assigning MAXFILES to a 
* large number we simply make the following statement in opencell.c & closecell.c 
* always FALSE.
* 	    if (fd >= MAXFILES)
*		......
* For questions or comments please contact:
* 			GPZ Technology, Inc.	June 1998
*			support@gpz.com
* Note: If you adopt the change made above, please do not remove this
* notes.  Thanks.
*/


/* if short is 16 bits, then
 *       short will allow 32767 cols
 *       unsigned short will allow 65536 cols
 * use int if you need more columns (but this will take more memory).
 *
*/
typedef int COLUMN_MAPPING ;

struct G__                         /*  Structure of library globals */
{
    struct Cell_head window ;        /* Contains the current window          */
    int window_set ;                 /* Flag: window set?                    */
    int mask_fd ;                    /* File descriptor for automatic mask   */
    int auto_mask ;                  /* Flag denoting automatic masking      */
    CELL *mask_buf;
    unsigned char *compressed_buf;   /* Pre/post compressed data buffer      */
    int compressed_buf_size ;        /* sizeof compressed_buf                */
    unsigned char *work_buf;         /* work data buffer                     */
    int work_buf_size ;              /* sizeof work_buf                      */
    int want_histogram ;

    struct fileinfo                  /* Information for opened cell files */
    {
        int open_mode ;            /* see defines below            */
        struct Cell_head cellhd ;  /* Cell header                  */
        struct Reclass reclass ;   /* Table reclass                */
	struct Cell_stats statf ;  /* Cell stats                   */
	struct Range range ;       /* Range structure              */
	int want_histogram ;
        int reclass_flag ;         /* Automatic reclass flag       */
        long *row_ptr ;            /* File row addresses           */
        COLUMN_MAPPING *col_map ;  /* Data to window col mapping   */
	double C1,C2;              /* Data to window row constants */
        int cur_row ;              /* Current data row in memory   */
	int cur_nbytes;            /* nbytes per cell for current row */
        unsigned char *data ;      /* Decompressed data buffer     */
        int nbytes;                /* bytes per cell               */
        char *temp_name ;          /* Temporary name for NEW files */
        char *name ;               /* Name of open file            */
        char *mapset ;             /* Mapset of open file          */
        int io_error ;             /* io error warning given       */
    }
    fileinfo[MAXFILES] ;
};

extern struct G__ G__ ;     /* allocated in gisinit */

long lseek();

#define OPEN_OLD              1
#define OPEN_NEW_COMPRESSED   2
#define OPEN_NEW_UNCOMPRESSED 3
#define OPEN_NEW_RANDOM       4
