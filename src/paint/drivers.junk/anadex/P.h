
#include <stdio.h>

struct PRINTER
{
    int fd;		/* file descriptor */
    int b;		/* buf counter */
    int bufsize;	/* buf size */
    unsigned char *buf;	/* output buffer */
    int tty;		/* is a tty? */
} ;


#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL struct PRINTER printer ;
GLOBAL int nrows, ncols ;
GLOBAL int darken ;
GLOBAL int ras_row;
GLOBAL unsigned char YELLOW[2][500];
GLOBAL unsigned char CYAN[2][500];
GLOBAL unsigned char MAJENTA[2][500];

#define CR_WITHOUT_LF ((char) 23)
