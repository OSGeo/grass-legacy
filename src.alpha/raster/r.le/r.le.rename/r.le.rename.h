#include "sys/types.h"


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

struct CHOICE {
       int  fcount ;
       int  process_all   ;
       char extension[80] ; 
       char old_fnames[30][30] ;
       char new_fnames[30][30] ;
};
