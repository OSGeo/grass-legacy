#include "include.h"
#include <stdlib.h>

int save (char *name)
{
    char anal_name[100] ;
    int i,n;
    int map ;
    FILE *fd ;

    if (*name)
    {
	G_ascii_check(name) ;
	if ((fd = fopen(name, "w")) == NULL)
	{
	    fprintf (stdout,"    Can't open %s\n", name) ;
	    return(-1) ;
	}
    }
    else
    {
	name = anal_name;
	while (1)
	{
	    fprintf (stdout,"By what name would you like the current analysis saved? > ") ;
	    if(sscanf(GETS,"%s", name) != 1)
		return 0;
	    G_ascii_check(name) ;
	    if ((fd = fopen (name, "w")) == NULL)
		fprintf (stdout,"    Can't open %s\n", name) ;
	    else
		break;
	}
    }

    fprintf (fd, "# weighted analysis\n");
    for (map=0; map<MAX_MAPS; map++)
    {
	if (!mapdef[map].used)
	    continue;
	fprintf (fd, "%s %s\n", mapdef[map].name, mapdef[map].mapset);
	n = G_get_histogram_num (&mapdef[map].histo);
	for (i = 0; i < n; i++)
	    fprintf (fd, "%ld:%ld\n",
		(long) G_get_histogram_cat (i, &mapdef[map].histo),
		G_get_histogram_count (i, &mapdef[map].histo));
	fprintf (fd, "#\n");
    }
    fclose (fd) ;
    fprintf (stdout,"    Current analysis table saved in file: %s\n", name) ;
    return(0) ;
}


int 
recover (char *name)
{
    char buf[1024];
    char anal_name[100] ;
    int map ;
    long cat;
    long count;
    int ok;
    int skip;
    FILE *fd ;

    if (*name)
    {
	G_ascii_check(name) ;
	if ((fd = fopen(name, "r")) == NULL)
	{
	    fprintf (stdout,"    Can't open %s\n", name) ;
	    return(-1) ;
	}
    }
    else
    {
	name = anal_name;
	while (1)
	{
	    fprintf (stdout,"Which analysis would you like recovered? > ") ;
	    if(sscanf(GETS,"%s", name) != 1)
		return -1;
	    G_ascii_check(name) ;
	    if ((fd = fopen(name, "r")) == NULL)
	    {
		fprintf (stdout,"    Can't open %s\n", name) ;
		fprintf (stdout,"    Files available:\n") ;
		system("ls") ;
	    }
	    else
		break ;
	}
    }

    for (map=0; map<MAX_MAPS; map++)
    {
	if (mapdef[map].used)
	    release_map (map);
    }

    map = -1;
    ok = (fgets(buf, sizeof buf, fd) && (*buf == '#'));
    while (ok && fgets (buf, sizeof buf, fd))
    {
	map++;
	if (map >= MAX_MAPS)
	{
	    fprintf (stdout,"   Warning: saved analysis contains more maps than weight allows\n");
	    fprintf (stdout,"   extra maps ignored\n");
	    break;
	}
	ok = (sscanf (buf, "%s %s", mapdef[map].name, mapdef[map].mapset) == 2);
	if (!ok) break;
	if (skip = (G_find_cell (mapdef[map].name, mapdef[map].mapset) == NULL))
	    fprintf (stdout,"  Warning: map [%s in %s] not found\n",
		mapdef[map].name, mapdef[map].mapset) ;
	else
	{
	    mapdef[map].used = 1;
	    G_init_histogram (&mapdef[map].histo);
	    G_set_histogram ( (CELL)0, (long)0, &mapdef[map].histo);
	}

	while (fgets (buf, sizeof buf, fd))
	{
	    if (*buf == '#') break;
	    if (skip) continue;
	    if (sscanf (buf, "%ld:%ld", &cat, &count) != 2)
	    {
		ok = 0;
		break;
	    }
	    G_set_histogram ( (CELL) cat, count, &mapdef[map].histo);
	}
	if (!skip)
	    G_sort_histogram (&mapdef[map].histo);
	if (!ok) break;
    }
    fclose (fd);
    if(!ok)
    {
	fprintf (stdout,"    error: bad read on analysis recover\n") ;
	fprintf (stdout,"    likely that %s is not a saved analysis\n", name) ;
    }
    else
	fprintf (stdout,"    Current analysis table recovered from file: %s\n", name) ;
    return(0) ;
}
