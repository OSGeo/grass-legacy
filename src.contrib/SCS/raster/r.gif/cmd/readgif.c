
/* Procedures and Variables to read page data out of GIF file */

#include        <stdio.h>
#include	<string.h>

#include	"readgif.h"

extern FILE *gif_file;

extern void close_stream()
{
   fclose( gif_file );
}/*close_stream */


extern short int	next_GIF_byte()
{
    if ( feof( gif_file ) ) return -1;
    return (short int)(fgetc( gif_file ));
} /* next_GIF_byte */

