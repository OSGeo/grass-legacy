#include "gis.h"

#define STATS "r.stats"
#define RECLASS "r.reclass"

main(argc, argv) char *argv[];
{
    char *me;
    char command[1024];
    char *mapset;
    struct Option *basemap, *covermap, *outputmap;
    struct Flag *flag_c;
    struct Categories cats;
    long catb, basecat, covercat;
    double x, area, sum1, sum2;
    int stat, usecats;
    char *tempfile1, *tempfile2;
    FILE *fd1, *fd2;

    G_gisinit(me=argv[0]);

    basemap = G_define_option();
    basemap->key        = "base";
    basemap->type       = TYPE_STRING ;
    basemap->required   = YES ;
    basemap->gisprompt  = "old,cell,raster" ;
    basemap->description = "base raster map";

    covermap = G_define_option();
    covermap->key       = "cover";
    covermap->type      = TYPE_STRING;
    covermap->required  = YES ;
    covermap->gisprompt  = "old,cell,raster" ;
    covermap->description = "cover raster map";

    outputmap = G_define_option();
    outputmap->key       = "output";
    outputmap->type      = TYPE_STRING;
    outputmap->required  = YES ;
    outputmap->gisprompt  = "new,cell,raster" ;
    outputmap->description = "resultant raster map";

    flag_c = G_define_flag();
    flag_c->key = 'c';
    flag_c->description = "cover values extracted from the category labels of the cover map";

    if (G_parser(argc,argv))
	exit(1);

    if (!G_find_cell(basemap->answer,""))
    {
	fprintf (stderr, "%s: %s - raster map not found\n", me, basemap->answer);
	exit(1);
    }
    if (!(mapset = G_find_cell(covermap->answer,"")))
    {
	fprintf (stderr, "%s: %s - raster map not found\n", me, covermap->answer);
	exit(1);
    }
    if (G_legal_filename(outputmap->answer) < 0)
    {
	fprintf (stderr, "%s: %s - illegal name\n", me, outputmap->answer);
	exit(1);
    }

    if(usecats = flag_c->answer)
    {
	if (G_read_cats (covermap->answer, mapset, &cats) < 0)
	{
	    fprintf (stderr, "%s: ERROR reading category file for %s\n",
		me, covermap->answer);
	    exit(1);
	}
    }

    tempfile1 = G_tempfile();
    tempfile2 = G_tempfile();
    sprintf (command, "%s -az input='%s,%s' fs=space > %s",
	STATS, basemap->answer, covermap->answer, tempfile1);
    if (stat = system(command))
    {
	unlink(tempfile1);
	fprintf (stderr, "%s: ERROR running %s command\n", me, STATS);
	exit(stat);
    }

    fd1 = fopen (tempfile1, "r");
    fd2 = fopen (tempfile2, "w");
    if (fd1 == NULL || fd2 == NULL)
    {
	unlink(tempfile1);
	unlink(tempfile2);
	fprintf (stderr, "%s: ERROR can't open tempfile\n", me);
	exit(1);
    }
    out(fd2, 0L, 0.0, 1.0);	/* force at least one reclass rule */

    catb = 0;
    sum1 = 0.0;
    sum2 = 0.0;
    while (fscanf (fd1, "%ld %ld %lf", &basecat, &covercat, &area) == 3)
    {
	if (catb != basecat)
	{
	    out(fd2, catb, sum1, sum2);
	    sum1 = 0.0;
	    sum2 = 0.0;
	    catb = basecat;
	}
	if (usecats)
	    sscanf (G_get_cat((CELL)covercat, &cats), "%lf", &x);
	else
	    x = covercat;
	sum1 += x * area;
	sum2 += area;
    }
    out(fd2, basecat, sum1, sum2);
    fclose (fd1);
    fclose (fd2);
    sprintf (command, "%s input='%s' output='%s' < %s",
	RECLASS, basemap->answer, outputmap->answer, tempfile2);
    stat = system(command);
    unlink (tempfile1);
    unlink (tempfile2);
    exit(stat);
}
out(fd, cat, sum1, sum2)
    FILE *fd;
    long cat;
    double sum1, sum2;
{
    char buf[80];

    if (sum2 == 0) return;
    if (cat == 0)
	*buf = 0;
    else
    {
	sprintf (buf, "%.10lf", sum1/sum2);
	G_trim_decimal (buf);
    }
    fprintf (fd, "%ld = %ld %s\n", cat, cat, buf);
}
