#include "digit.h"

/*
**  watch how routines work if A == B.  Will have to make this work
**  on a line against itself
*/

static struct line_pnts Points;

char *malloc (), *realloc ();
char *getenv ();

debug_graphic (map, A, B, X, Y)
    struct Map_info *map;
    double X, Y;
{
    char *p, buf[300];
    FILE *fp, *popen ();
    double n, s, e, w;
    double diff;
    static int first = 1;


    /* env  GRAPH must be set to execute this */
    if (NULL == getenv ("GRAPH"))
	return;

    g_reset_window ();

    if (first)
    {
	first = 0;
	Points.alloc_points = 0;
    }

    g_graph_line (map, A, B, X, Y);

    while (1)
    {
	printf ("\nc a b l w W q Q: ");
	if (NULL == gets (buf))
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

g_set_window (N, S, E, W)
    double N, S, E, W;
{
    Sn = N, Ss = S, Se = E, Sw = W;
    g_reset_window ();
}

g_reset_window ()
{
    char buf[200];

    /*
    sprintf (buf, "Gwindow nsres=%lf ewres=%lf n=%lf s=%lf e=%lf w=%lf", 
	(Sn-Ss)/100., (Se-Sw)/100., Sn, Ss, Se, Sw);
    system (buf);
    */

    system ("Gwindow window=swirl");

    system ("Derase");  /* to reset new window */
}

reset_d_window ()
{
    system ("d.window");

    system ("Derase");  /* to reset new window */
}


g_graph_line (map, A, B, x, y)
    struct Map_info *map;
    double x, y;
{
    FILE *fp;
    int loop, i, line;
   
    if (NULL == (fp = popen ("Dmapgraph", "w")))
    {
	fprintf (stderr, "popen failed\n");
	return;
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

	fprintf (fp, "icon x 2 %lf %lf\n", Points.x[0], Points.y[0]);
	fprintf (fp, "move %lf %lf\n", Points.x[0], Points.y[0]);

	for (i = 1 ; i < Points.n_points ; i++)
	{
	    fprintf (fp, "draw %lf %lf\n", Points.x[i], Points.y[i]);
	    fprintf (fp, "icon o 1 %lf %lf\n", Points.x[i], Points.y[i]);
	}
	fprintf (fp, "icon x 2 %lf %lf\n", Points.x[Points.n_points-1], Points.y[Points.n_points]);
    }
    fprintf (fp, "color yellow\n");
    /*fprintf (fp, "icon x 4 %lf %lf\n", x, y);*/
    fprintf (fp, "icon 0 1 %lf %lf\n", x, y);
    pclose (fp);
}
