/*  Written by Dave Gerdes CERL   11/90 */

/*
 * Start up graphics processing.  Anything that needs to be assigned, set up,
 * started-up, or otherwise initialized happens here.  This is called only at
 * the startup of the graphics driver.
 *
 * The external variables define the pixle limits of the graphics surface.  The
 * coordinate system used by the applications programs has the (0,0) origin
 * in the upper left-hand corner.  Hence,
 *    screen_left < screen_right
 *    screen_top  < screen_bottom 
 *
 * NCOLORS is set to the total number of colors available on the device.  This
 * most certainly needs to be more than 100 (or so).  If you are writing a
 * driver with fewer colors you probably need to provide your own Color(),
 * Color_table_float(), Color_table_fixed(), and
 * Reset_color() routines (see ../lib/{Color.c,Reset_clr.c,Clr_table.c}).
 */

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "driverlib.h"


#define MAIN
#include "cell.h"

int screen_left;
int screen_top;
int screen_right;
int screen_bottom;
int NCOLORS       = 256 ;

#define BUFSIZE 10*BUFSIZ


int Graph_Set (int argc, char **argv, int nlev) 
{
    char *p;
    char buf[1024];
    long f_size, file_size, bufsize;
    int i, j;

    G_gisinit("CELL driver") ;

    if (NULL != (p = getenv ("GRASS_WIDTH")))
	screen_right = atoi (p);
    else
	screen_right = DEF_WIDTH;

    Cur_color = 0;

    if (NULL != (p = getenv ("GRASS_HEIGHT")))
	screen_bottom = atoi (p);
    else
	screen_bottom = DEF_HEIGHT;

    /* clear out color table */
    for (i = 0 ; i < 256 ; i++)
	for (j = 0 ; j < 3 ; j++)
	    Color_table[i][j] = 0;

    strcpy (buf, p);
    strcat (buf, FILE_NAME);
    Filename = G_store (buf);

    /* alloc tmp buffer for num_cols */
    Row_buf = (unsigned char *) G_malloc (screen_right - screen_left);


    file_size = (screen_right - screen_left) * (screen_bottom - screen_top);

    Filename = G_tempfile();
    if ((Temp_fp = fopen (Filename, "w+")) == NULL)


/*DEBUG*/ fprintf (stderr, "Size of tmpfile %ld:\n", file_size);
    fprintf (stderr, "Wait for 'READY'\n");
    /* init file to all 0s */
    f_size = file_size;
    p = G_calloc (BUFSIZE, 1);
    while (f_size > 0)
    {
/*DEBUG	fprintf (stderr, "Bytes Left %d\n", f_size);*/
	if (f_size > BUFSIZE)
	    bufsize = BUFSIZE;
	else
	    bufsize = f_size;
	fwrite (p, 1, bufsize, Temp_fp);
	f_size -= bufsize;
    }
    free (p);
/*DEBUG*/ fprintf (stderr, "READY\n");

   return 0;
}
