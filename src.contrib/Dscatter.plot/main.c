#include "gis.h"

struct corr
{
    long a,b;
    long count;
} ;

main(argc, argv) char *argv[];
{
    char buf[1024];
    struct corr *corr;
    int i,
	len,
	top, bottom, left, right,
	backr, backg, backb,
	ncorr;
    FILE *fd;
    int cmp();
    long mina, maxa,
	 minb, maxb,
	 mincount, maxcount,
	 a,b,count;
    struct Cell_head window;
    struct Colors colr;
    int row;
    CELL *cell;
    double scale;

    G_gisinit (argv[0]);

    if (argc < 3 || argc > 4)
    {
	fprintf (stderr, "Usage: %s layer1 layer2 [colortype]\n", argv[0]);
	exit(1);
    }
    i = 0;
    if (!G_find_cell (argv[1], ""))
    {
	fprintf (stderr, "%s: cell file not found\n", argv[1]);
	i = 1;
    }
    if (!G_find_cell (argv[2], ""))
    {
	fprintf (stderr, "%s: cell file not found\n", argv[2]);
	i = 1;
    }
    if (i)
	exit(1);

    R_open_driver();
    Derase (D_translate_color("black"));

    sprintf (buf, "Gstats -c '%s' '%s'", argv[1], argv[2]);
    fd = popen (buf, "r");
    if (fd == NULL)
	exit(1);

    corr = NULL;
    ncorr = 0;

    while ((i = fscanf (fd, "%ld:%ld:%ld", &a, &b, &count)) == 3)
    {
	if (a == 0 || b == 0) continue;
	ncorr++;
	corr = (struct corr *) G_realloc (corr, ncorr*sizeof(struct corr));
	corr[ncorr-1].a = a;
	corr[ncorr-1].b = b;
	corr[ncorr-1].count = count;
    }
    pclose (fd);
    /*
    qsort (corr, ncorr, sizeof(struct corr), cmp);
    */

    mina=maxa=corr[0].a;
    minb=maxb=corr[0].b;
    mincount = maxcount = corr[0].count;

    for (i = 1; i < ncorr; i++)
    {
	if (corr[i].a > maxa) maxa = corr[i].a;
	if (corr[i].a < mina) mina = corr[i].a;
	if (corr[i].b > maxb) maxb = corr[i].b;
	if (corr[i].b < minb) minb = corr[i].b;
	if (corr[i].count > maxcount) maxcount = corr[i].count;
	if (corr[i].count < mincount) mincount = corr[i].count;
    }
/* counts will be scales to 0-255 for color intensities */
    if (mincount == maxcount)
	scale = 1.0;
    else
	scale = 255.0 / (maxcount-mincount);
    for (i = 1; i < ncorr; i++)
	corr[i].count *= scale;

    window.ns_res = window.ew_res = 1.0;
    window.north  = maxa + .5;
    window.south  = mina - .5;
    window.east   = maxb + .5;
    window.west   = minb - .5;
    window.proj   = G_projection();
    window.zone   = G_zone();
    G_set_window (&window);

    backr = backg = backb = 127;
    if (argc == 4)
    {
	if (strcmp (argv[3], "ryg") ==0)
	    G_make_red_yel_grn (&colr, (CELL) 1, (CELL) 255);
	else if (strcmp (argv[3], "ramp") ==0)
	    G_make_color_ramp (&colr, (CELL) 1, (CELL) 255);
	else if (strcmp (argv[3], "wave") ==0)
	    G_make_color_wave (&colr, (CELL) 1, (CELL) 255);
	else if (strcmp (argv[3], "rainbow") ==0)
	    G_make_rainbow_colors (&colr, (CELL) 1, (CELL) 255);
	else if (strcmp (argv[3], "grey") ==0)
	{
	    backr = 0;
	    backg = 0;
	    backb = 187;
	    G_make_grey_scale (&colr, (CELL) 1, (CELL) 255);
	}
	else
	    G_make_rainbow_colors (&colr, (CELL) 1, (CELL) 255);
    }
    else
	G_make_rainbow_colors (&colr, (CELL) 1, (CELL) 255);
    G_set_color ((CELL) 0, backr, backg, backb, &colr);

    D_check_map_window (&window);

    D_get_screen_window (&top, &bottom, &left, &right);
    if (D_cell_draw_setup (top, bottom, left, right))
    {
	R_close_driver();
	fprintf (stderr, "Problem with current graphics window\n");
	exit(1);
    }
    D_reset_colors (&colr);
    G_free_colors (&colr);

    cell = G_allocate_cell_buf();
    for (row = 0; row != -1;)
    {
	a = maxa - row;
	if (a > maxa || a < mina) break;	/* just to be safe */
	G_zero_cell_buf (cell);
	for (i = 0; i < ncorr; i++)
	{
	    if (corr[i].a == a)
		cell[corr[i].b - minb] = corr[i].count;
	}
	row = D_draw_cell_row (row, cell);
    }
    R_close_driver();
    if(fd = popen("Dtext size=5","w"))
    {
	len = strlen (argv[1]);
	i = strlen (argv[2]);
	if (i > len) len = i;
	fprintf (fd, "Scatter plot %s %s\n", argv[1], argv[2]);
	fprintf (fd, " %*s: min %ld, max %ld\n", len, argv[1], mina, maxa);
	fprintf (fd, " %*s: min %ld, max %ld\n", len, argv[2], minb, maxb);
	pclose (fd);
    }
}

cmp (s1, s2)
    struct corr *s1, *s2;
{
    if (s1->a < s2->a) return -1;
    if (s1->a > s2->a) return 1;
    if (s1->b < s2->b) return -1;
    if (s1->b > s2->b) return 1;
    return 0;
}
