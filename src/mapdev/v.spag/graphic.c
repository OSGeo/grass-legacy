#include <stdlib.h>
#include "Vect.h"
#include "Vect.h"
#include "local_proto.h"

/*
**  watch how routines work if A == B.  Will have to make this work
**  on a line against itself
*/

static struct line_pnts Points;

int debug_graphic (struct Map_info *map,int A,int B,double X,double Y)
{
    char *p, buf[300];
    static int first = 1;


    /* env  GRAPH must be set to execute this */
    if (NULL == getenv ("GRAPH"))
	return 1;

    g_reset_window ();

    if (first)
    {
	first = 0;
	Points.alloc_points = 0;
    }

    g_graph_line (map, A, B, X, Y);

    while (1)
    {
	fprintf (stdout,"\nc a b l w W q Q: ");
	if (fgets (buf,300,stdin)==NULL)
	    break;
	p = buf;
	while (*p)
	{
	    switch (*p++) {
		case 'c':  /* clear */
		    system ("Derase");
		    break;
		case 'a':   /* draw line A */
		    g_graph_line (map, A, 0, X, Y);
		    break;
		case 'l':   /* draw both lines */
		    g_graph_line (map, A, B, X, Y);
		    break;
		case 'b':   /* draw line B */
		    g_graph_line (map, 0, B, X, Y);
		    break;
		case 'w':  /* rewindow */
		    reset_d_window ();
		    break;
		case 'W':  /* rewindow */
		    reset_window ();
		    break;
		case 'q': /* quit */
		    goto done;
		    break;
		case 'Q':  /* exit */
		    exit (0);
		    break;
		case '\r':
		case '\n':
		    goto done;
		default: 
		    break;
	    }
	}
    }
done:

    return 0;
}

static double Sn, Ss, Se, Sw;

int g_set_window (double N, double S, double E, double W)
{
    Sn = N, Ss = S, Se = E, Sw = W;
    g_reset_window ();

    return 0;
}

int 
g_reset_window (void)
{
    /*
    sprintf (buf, "Gwindow nsres=%f ewres=%f n=%f s=%f e=%f w=%f", 
	(Sn-Ss)/100., (Se-Sw)/100., Sn, Ss, Se, Sw);
    system (buf);
    */

    system ("Gwindow window=swirl");

    system ("Derase");  /* to reset new window */

    return 0;
}

int reset_d_window (void)
{
    system ("d.window");

    system ("Derase");  /* to reset new window */

    return 0;
}


int g_graph_line (struct Map_info *map, int A, int B, double x, double y)
{
    FILE *fp;
    int loop, i, line;
   
    if (NULL == (fp = popen ("Dmapgraph", "w")))
    {
	fprintf (stderr, "popen failed\n");
	return 1;
    }

    for (loop = 0 ; loop < 2 ; loop++)
    {
	line = loop ? B : A;

	if (line)
	{
	    if (0 > (V1_read_line (map, &Points, map->Line[line].offset)))
		return (-1);
	}
	else continue;

	if (loop == 0)
	    fprintf (fp, "color gray\n");
	else
	    fprintf (fp, "color blue\n");

	fprintf (fp, "icon x 2 %f %f\n", Points.x[0], Points.y[0]);
	fprintf (fp, "move %f %f\n", Points.x[0], Points.y[0]);

	for (i = 1 ; i < Points.n_points ; i++)
	{
	    fprintf (fp, "draw %f %f\n", Points.x[i], Points.y[i]);
	    fprintf (fp, "icon o 1 %f %f\n", Points.x[i], Points.y[i]);
	}
	fprintf (fp, "icon x 2 %f %f\n", Points.x[Points.n_points-1], Points.y[Points.n_points]);
    }
    fprintf (fp, "color yellow\n");
    /*fprintf (fp, "icon x 4 %f %f\n", x, y);*/
    fprintf (fp, "icon 0 1 %f %f\n", x, y);
    pclose (fp);

    return 0;
}
