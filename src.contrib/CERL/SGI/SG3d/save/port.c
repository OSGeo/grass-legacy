
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include "gis.h"
#include "externs.h"


FILEDESC
G_open_cell_old(name)
    char *name;
{
    FILEDESC fd;
    char buf[200];

    sprintf (buf, "../data/%s", name);
    if (NULL  == (fd = open (buf,0)))
	fprintf (stderr, "Cant open file '%s'\n", buf), exit (-1);
    return (fd);
}


G_get_set_window (wind) 
    struct Cell_head *wind;
{
    /* ORIGINAL DATA
    wind->north =         4928320.30;
    wind->south =         4922848.30;
    wind->east  =          601705.43;
    wind->west  =          588628.29;
    wind->ns_res =               6.40;
    wind->ew_res =               6.26;
    wind->rows  =             HEIGHT;
    wind->cols  =            WIDTH;
    */

    /*  Real window */
    /*
    wind->north =        4925570.00;
    wind->south =        4923050.00;
    wind->east =          592320.00;
    wind->west =          589980.00;
    wind->ns_res =              5.00;
    wind->ew_res =              5.00;
    wind->rows =             W_ROWS;
    wind->cols =             W_COLS;
    */

/*
#define W_ROWS 504
#define W_COLS 468
*/

    wind->north =           5570.00;
    wind->south =           3050.00;
    wind->east =            9320.00;
    wind->west =            6980.00;
    wind->ns_res =              5.00;
    wind->ew_res =              5.00;
    wind->rows =             W_ROWS;
    wind->cols =             W_COLS;
}

G_close_cell (fd)
    FILEDESC fd;
{
    close (fd);
}

char *
G_malloc (size)
{
    char *p;

    if (NULL == (p = malloc (size)))
	fprintf (stderr, "Out of memory \n"), exit (-1);
    
    return p;
}

G_get_map_row(fd, buf, row, size) /* hack! */
    FILEDESC fd;
    int *buf;
{
    short tbuf[WIDTH];
    unsigned char *p;
    int i;

    row += Y_OFFSET;
    lseek (fd, (long) row * (WIDTH * size), 0);
    if (size == 1)
    {
	read (fd, (unsigned char *)tbuf, WIDTH * size);
	p = (unsigned char *) tbuf;
	for (i = 0 ; i < W_COLS ; i++)
	    buf[i] = p[i+X_OFFSET];
    }
    else
    {
	read (fd, tbuf, WIDTH * size);
	for (i = 0 ; i < W_COLS ; i++)
	    buf[i] = tbuf[i+X_OFFSET];
    }
    return (0);
}

G_fatal_error (str)
    char *str;
{
    fprintf (stderr, "Fatal Error: %s\n", str);
    exit (-1);
}

char *
G_store (str)
    char *str;
{
    char *p;

    strcpy ((p = G_malloc (strlen (str)+1)), str);
    return (p);
}
