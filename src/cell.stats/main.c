#include "gis.h"
#include "conv.h"
/*---------------------------------------------------------------------
 |      cell_stats()
 |		Cell Layer Conversion Statistics	
 |
 */

main (argc, argv)	char *argv[];
{
    double      square_meters ;
    double      acres, hectacres, miles;
    double      acres0, hectacres0, miles0;
    double      value;
    char        line[80];
    char	name[50];
    char        buf1[80], buf2[80], buf3[80];
    char        *mapset;
    char	*tempfile;
    FILE	*fopen(),*reportfd, *fd;
    struct Categories cats;
    struct Cell_head window;
    long x;
    int unexpected_cats;
    char *G_get_cat ();

    G_gisinit(argv[0]);

    G_get_window (&window);


    acres = 0.0;
    hectacres = 0.0;
    miles = 0.0;
    acres0 = 0.0;
    hectacres0 = 0.0;
    miles0 = 0.0;

/* prompt for cell file */
    mapset = G_ask_cell_old ("",name);
    if(!mapset)
	exit(0);

/* get the category names and cell title */
    if (G_read_cats (name, mapset, &cats) < 0)
	exit(-1);

/* run Gstats to read the cell file */
    sprintf (line, "Gstats '%s in %s'", name, mapset);
    fd = popen (line, "r");
    if (fd == NULL)
	G_fatal_error ("unable to run Gstats");

/****************************************************************/

/* write the report to the temp file */

    tempfile = G_tempfile () ;
    reportfd = fopen(tempfile,"w");
    if (!reportfd)
    {
	perror (tempfile);
	exit(1);
    }


    fprintf (reportfd, " +--------------------------------------");
    fprintf (reportfd, "--------------------------------------+\n");

    sprintf (line,    "Layer:   [%s] in mapset [%s]", name, mapset);
    fprintf (reportfd, " | %-74s |\n", line);

    sprintf (line,     "Title:   %s", cats.title);
    fprintf (reportfd, " | %-74.74s |\n", line);

    sprintf (line,     "Mask:    %s", G_mask_info());
    fprintf (reportfd, " | %-74.74s |\n", line);

    fprintf (reportfd, " |--------------------------------------");
    fprintf (reportfd, "--------------------------------------|\n");

    G_format_northing (window.north, buf1, window.proj);
    G_format_easting  (window.east,  buf2, window.proj);
    sprintf (line, "          north: %10s       east: %10s", buf1, buf2);
    fprintf (reportfd, " | %-74s |\n", line);

    G_format_northing (window.south, buf1, window.proj);
    G_format_easting  (window.west,  buf2, window.proj);
    sprintf (line, "Window:   south: %10s       west: %10s", buf1, buf2);
    fprintf (reportfd, " | %-74s |\n", line);

    G_format_resolution (window.ns_res, buf1, window.proj);
    G_format_resolution (window.ew_res, buf2, window.proj);
    sprintf (line, "          res:   %10s       res:  %10s", buf1, buf2);
    fprintf (reportfd, " | %-74s |\n", line);


    fprintf (reportfd, " |--------------------------------------");
    fprintf (reportfd, "--------------------------------------|\n");
    fprintf (reportfd, " |    Category                              ");
    fprintf (reportfd, "   Acres     Hectares      Sq.mi  |\n");
    fprintf (reportfd, " |--------------------------------------");
    fprintf (reportfd, "--------------------------------------|\n");

    unexpected_cats = 0;
    while (fscanf (fd, "%ld:%lf", &x, &square_meters) == 2)
    {
	char *label;
	label = G_get_cat ((CELL)x, &cats);
	if (*label == 0 && x > cats.num)
	{
	    unexpected_cats++;
	    label = "** unexpected category **";
	}
	fprintf (reportfd, " |%4ld    ", x);
	fprintf (reportfd, "%-32.30s", label);

	value =	ACRES (square_meters);
	acres += value;
	if (x == 0) acres0 = value;
	format_double (value, buf1, 10);
	fprintf (reportfd, "%10s  ", buf1);

	value =	HECTACRES (square_meters);
	hectacres += value;
	if (x == 0) hectacres0 = value;
	format_double (value, buf1, 10);
	fprintf (reportfd, "%10s  ", buf1);

	value = MILES (square_meters);
	miles += value;
	if (x == 0) miles0 = value;
	format_double (value, buf1, 10);
	fprintf (reportfd, "%10s  ", buf1);

	fprintf (reportfd, "|\n");
    }
    pclose (fd);
    fprintf (reportfd," |%40s----------  ----------  ----------  |\n","");

    format_double (acres, buf1, 10);
    format_double (hectacres, buf2, 10);
    format_double (miles, buf3, 10);

    fprintf (reportfd," |%9s%-31s%10s  %10s  %10s  |\n",
	    "","total", buf1, buf2, buf3);

    if (acres0 != 0.0)
    {
	acres     -= acres0 ;
	hectacres -= hectacres0 ;
	miles     -= miles0 ;

	format_double (acres, buf1, 10);
	format_double (hectacres, buf2, 10);
	format_double (miles, buf3, 10);

	fprintf (reportfd," |%9s%-31s%10s  %10s  %10s  |\n",
	    "","total without category 0", buf1, buf2, buf3);
    }

    fprintf (reportfd, " |                                     ");
    fprintf (reportfd, "                                       |\n");
    fprintf (reportfd, " +--------------------------------------");
    fprintf (reportfd, "--------------------------------------+\n\n");

    fclose(reportfd);
    more_print(tempfile, unexpected_cats);

    exit (0);
}
