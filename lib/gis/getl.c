#include "gis.h"
/*
 ***********************************************************
 *  G_getl(buf, n, fd)
 *     char *buf         buffer to receive read data
 *     int n             max num of bytes to read
 *     FILE *fd          file descriptor structure
 *
 *  does fgets() and removes trailing newline
 *  
 *  returns: 1 ok, 0 eof
 ************************************************************/

#include <stdio.h>
int G_getl ( char *buf, int n, FILE *fd)
{
    if (!fgets (buf, n, fd))
	return 0;

    for (; *buf && *buf != '\n'; buf++)
	    ;
    *buf = 0;

    return 1;
}

/*
 ***********************************************************
 *  G_getl(buf, n, fd)
 *     char *buf         buffer to receive read data, at least n+1 must be allocated
 *     int n             max num of bytes to read
 *     FILE *fd          file descriptor structure
 *
 *  Reads in at most n characters from stream and stores them into the  buffer pointed to by buf.
 *  Reading stops after an EOF or a newline. New line is not stored in the buffer.
 *  It supports text files created on various platforms (UNIX,DOS,Mac), 
 *  i.e. \n (\012), \r (\015) and \r\n (\015\012)
 *  
 *  returns: 1 ok, 0 eof
 ************************************************************/

#include <stdio.h>
int G_getl2 ( char *buf, int n, FILE *fd)
{
    int i = 0;
    int c;
    int ret = 1;

    while ( i < n  ) {
	c = fgetc(fd);

	if ( c == EOF ) { 
	    if ( i == 0 ) { /* Read correctly (return 1) last line in file without '\n' */
	        ret = 0;
	    }
	    break;
	}
	
	if ( c == '\012' ) break;               /* UNIX */

	if ( c == '\015' ) {                    /* DOS or MAC */
            if ( (c = fgetc(fd) ) != EOF ) {     
		if ( c != '\012' ) {            /* MAC - we have to return the char to stream */
		    ungetc ( c, fd );
		}
	    }
            break;
	}
	
        buf[i] = c;

	i++;
    }	
    buf[i] = '\0';

    G_debug ( 4, "G_getl2: ->%s<-", buf );

    return ret;
}

