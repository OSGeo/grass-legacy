#define GLOBAL
#include "global.h"

static char *me;
main(argc,argv) char *argv[];
{
    struct Colors colr;
    struct Categories cats;
    struct Ref group_ref;
    int nrows, ncols;
    int row;
    int band;
    int i,r,g,b;
    int red, grn, blu;

    G_gisinit (me = argv[0]);

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (G_maskfd() >= 0)
    {
	printf ("\nWARNING: you have your mask set.\n");
	if (!G_yes("Do you want to continue? ", -1)) exit(0);
    }
    ask_files (argv[0]);

    if (G_fork())
    {
	printf ("you will receive mail when %s is complete\n", argv[0]);
	exit(0);
    }
    freopen ("/dev/null", "w", stderr);

    class_fd = G_open_cell_new (class_name);
    if (class_fd < 0)
    {
	char msg[100];
	sprintf (msg, "unable to create layer [%s] in [%s]",
		class_name, G_mapset());
	G_fatal_error (msg);
    }
    if (reject_cell != NULL)
    {
	reject_fd = G_open_cell_new (reject_name);
	if (reject_fd < 0)
	{
	    char msg[100];
	    sprintf (msg, "unable to create layer [%s] in [%s]",
		    reject_name, G_mapset());
	    G_warning (msg);
	    free (reject_cell);
	    reject_cell = NULL;
	}
    }

    for (row = 0; row < nrows; row++)
    {
	for (band = 0; band < Ref.nfiles; band++)
	    if (G_get_map_row (cellfd[band], cell[band], row) < 0)
		exit(1);
	classify(class_cell, reject_cell, ncols);
	G_put_map_row (class_fd, class_cell, row);
	if (reject_fd > 0)
	    G_put_map_row (reject_fd, reject_cell, row);
    }
    G_close_cell (class_fd);
    if (reject_fd > 0)
	G_close_cell (reject_fd);

    G_init_cats((CELL)S.nsigs,"Maximum Likelihood Classification",&cats);
    for (i=0; i < S.nsigs; i++)
    {
	if(*S.sig[i].desc)
	    G_set_cat ((CELL)(i+1),S.sig[i].desc, &cats);
    }
    G_write_cats (class_name, &cats);
    G_free_cats (&cats);

    red = grn = blu = -1;
    if (have_colors)
    {
	if (Ref.red.table != NULL)
	    red = Ref.red.n;
	if (Ref.grn.table != NULL)
	    grn = Ref.grn.n;
	if (Ref.blu.table != NULL)
	    blu = Ref.blu.n;
    }

    G_init_colors (&colr);
    G_set_color ((CELL) 0, 0, 0, 0, &colr);
    for (i=0; i < S.nsigs;i++)
    {
	if (!S.sig[i].have_color)
	{
	    r = g = b = 0;
	    if (red >= 0)
	    {
		r = S.sig[i].mean[red] + .5;
		if (r < Ref.red.min)
		    r = 0;
		else if (r > Ref.red.max)
		    r = 255;
		else
		    r = Ref.red.table[r-Ref.red.min];
	    }
	    if (grn >= 0)
	    {
		g = S.sig[i].mean[grn] + .5;
		if (g < Ref.grn.min)
		    g = 0;
		else if (g > Ref.grn.max)
		    g = 255;
		else
		    g = Ref.grn.table[r-Ref.grn.min];
	    }
	    if (blu >= 0)
	    {
		b = S.sig[i].mean[blu] + .5;
		if (b < Ref.blu.min)
		    b = 0;
		else if (b > Ref.blu.max)
		    b = 255;
		else
		    b = Ref.blu.table[b-Ref.blu.min];
	    }
	}
	else
	{
	    r = S.sig[i].r;
	    g = S.sig[i].g;
	    b = S.sig[i].b;
	}
	G_set_color ((CELL)(i+1), r, g, b, &colr);
    }
    G_write_colors (class_name, G_mapset(), &colr);
    G_free_colors (&colr);

    if (reject_fd > 0)
    {
	char title[100];

	sprintf (title, "Rejection Probability for %s", class_name);

	G_init_cats((CELL)17,title,&cats);
	G_set_cats_title (title, &cats);
	G_set_cat ((CELL)0, "no data", &cats);
	G_set_cat ((CELL)1, "0.1%", &cats);
	G_set_cat ((CELL)2, "0.5%", &cats);
	G_set_cat ((CELL)3, "1%", &cats);
	G_set_cat ((CELL)4, "2%", &cats);
	G_set_cat ((CELL)5, "5%", &cats);
	G_set_cat ((CELL)6, "10%", &cats);
	G_set_cat ((CELL)7, "20%", &cats);
	G_set_cat ((CELL)8, "30%", &cats);
	G_set_cat ((CELL)9, "50%", &cats);
	G_set_cat ((CELL)10, "70%", &cats);
	G_set_cat ((CELL)11, "80%", &cats);
	G_set_cat ((CELL)12, "90%", &cats);
	G_set_cat ((CELL)13, "95%", &cats);
	G_set_cat ((CELL)14, "98%", &cats);
	G_set_cat ((CELL)15, "99%", &cats);
	G_set_cat ((CELL)16, "100%", &cats);
	G_set_cat ((CELL)17, "bad", &cats);
	G_write_cats (reject_name, &cats);
	G_free_cats (&cats);


	G_make_grey_scale_colors (&colr, (CELL) 1, (CELL) 16);

	G_set_color ((CELL)0, 0, 255, 0, &colr);
	G_set_color ((CELL)17, 255, 0, 0, &colr);
	G_write_colors (reject_name, G_mapset(), &colr);
	G_free_colors (&colr);
    }

/* associate the output files with the group */
    I_get_group_ref (group, &group_ref);
    I_add_file_to_group_ref (class_name, G_mapset(), &group_ref);
    if (reject_cell)
	I_add_file_to_group_ref (reject_name, G_mapset(), &group_ref);
    I_put_group_ref (group, &group_ref);

    done();
}
done()
{
    FILE *popen(), *mail;
    char buf[1024];

    sprintf (buf, "mail '%s'", G_whoami());
    mail = popen (buf,"w");
    if (mail)
    {
	fprintf (mail, "Subject: %s\n", me);
	fprintf (mail, "%s complete\n\n", me);
	fprintf (mail, "Location:       %s\n", G_location());
	fprintf (mail, "Mapset:         %s\n", G_mapset());
	fprintf (mail, "Group:          %s\n", group);
	fprintf (mail, "Subgroup:       %s\n", subgroup);
	fprintf (mail, "Signature file: %s\n\n", sigfile);
	fprintf (mail, "Classified map layer:       %s\n", class_name);
	if (reject_cell)
	fprintf (mail, "Reject threshold map layer: %s\n", reject_name);
	pclose (mail);
    }
    G_done_msg ("Check your mail");
}
