#include "gis.h"

#define STATS "r.stats"
#define RECLASS "r.reclass"

main(argc, argv) char *argv[];
{
    char *me;
    char command[1024];
    char *mapset;
    struct Option *basemap, *valuemap, *resultmap;
    struct Flag *flag_c;
    struct Categories cats;
    long cat0, cat1, cat2;
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

    valuemap = G_define_option();
    valuemap->key       = "values";
    valuemap->type      = TYPE_STRING;
    valuemap->required  = YES ;
    valuemap->gisprompt  = "old,cell,raster" ;
    valuemap->description = "raster map containing values to be averaged";

    resultmap = G_define_option();
    resultmap->key       = "result";
    resultmap->type      = TYPE_STRING;
    resultmap->required  = YES ;
    resultmap->gisprompt  = "new,cell,raster" ;
    resultmap->description = "resultant raster map";

    flag_c = G_define_flag();
    flag_c->key = 'c';
    flag_c->description = "values produced from category labels for values map";

    if (G_parser(argc,argv))
	exit(1);

    if (!G_find_cell(basemap->answer,""))
    {
	fprintf (stderr, "%s: %s - raster map not found\n", me, basemap->answer);
	exit(1);
    }
    if (!(mapset = G_find_cell(valuemap->answer,"")))
    {
	fprintf (stderr, "%s: %s - raster map not found\n", me, valuemap->answer);
	exit(1);
    }
    if (G_legal_filename(resultmap->answer) < 0)
    {
	fprintf (stderr, "%s: %s - illegal name\n", me, resultmap->answer);
	exit(1);
    }

    if(usecats = flag_c->answer)
    {
	if (G_read_cats (valuemap->answer, mapset, &cats) < 0)
	{
	    fprintf (stderr, "%s: ERROR reading category file for %s\n",
		me, valuemap->answer);
	    exit(1);
	}
    }

    tempfile1 = G_tempfile();
    tempfile2 = G_tempfile();
    sprintf (command, "%s -az input='%s,%s' fs=space > %s",
	STATS, basemap->answer, valuemap->answer, tempfile1);
    if (stat = system(command))
    {
	fprintf (stderr, "%s: ERROR running %s command\n", me, STATS);
	exit(stat);
    }

    fd1 = fopen (tempfile1, "r");
    fd2 = fopen (tempfile2, "w");
    if (fd1 == NULL || fd2 == NULL)
    {
	fprintf (stderr, "%s: ERROR can't open tempfile\n", me);
	exit(1);
    }

    cat0 = 0;
    sum1 = 0.0;
    sum2 = 0.0;
    while (fscanf (fd1, "%ld %ld %lf", &cat1, &cat2, &area) == 3)
    {
	if (cat0 != cat1)
	{
	    out(fd2, cat0, sum1, sum2);
	    sum1 = 0.0;
	    sum2 = 0.0;
	    cat0 = cat1;
	}
	if (usecats)
	    sscanf (G_get_cat((CELL)cat2, &cats), "%lf", &x);
	else
	    x = cat2;
	sum1 += x * area;
	sum2 += area;
    }
    out(fd2, cat1, sum1, sum2);
    fclose (fd1);
    fclose (fd2);
    sprintf (command, "%s input='%s' output='%s' < %s",
	RECLASS, basemap->answer, resultmap->answer, tempfile2);
    stat = system(command);
    exit(stat);
}
out(fd, cat, sum1, sum2)
    FILE *fd;
    long cat;
    double sum1, sum2;
{
    if (sum2 != 0.0)
	fprintf (fd, "%ld = %ld %lf\n", cat, cat, sum1/sum2);
}
