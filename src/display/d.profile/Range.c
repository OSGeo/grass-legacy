
#include "gis.h"
#include <stdio.h> 

WindowRange(name, mapset, min, max)
char *name,
     *mapset;
int  *min,
     *max;
{
char              inbuf[512];     /* input buffer for reading stats */
int               done = 0;
char              stats_cmd[512];  /* string for r.stats command */
char              *temp_fname;     /* temp file name */
FILE              *temp_file;      /* temp file pointer */
long int          cat;             /* a category value */
long int          stat;            /* a category stat value */
int               first;

/* write stats to a temp file */
temp_fname = G_tempfile();
sprintf(stats_cmd,"r.stats -c %s > %s\n",name,temp_fname);
system(stats_cmd);

/* open temp file and read the stats into a linked list */
temp_file = fopen(temp_fname,"r");

first=1;
while (!done)
   {
   if (fgets(inbuf,1024,temp_file) != NULL)
      {
      if (sscanf(inbuf,"%ld %ld",&cat,&stat) == 2)
         {
         if (first)
            {
	    *max = cat; 
	    *min = cat; 
            first = 0; 
            }
         else 
            {
            if (cat > *max)
               *max = cat; 
            if (cat < *min)
               *min = cat; 
            }
         }
      else done = 1;
      }
   else done = 1;
   }
}

quick_range (name, mapset, min, max)
    char *name, *mapset;
    long *min, *max;
{
    struct Range range;
    CELL xmin,xmax;

    if (G_read_range (name, mapset, &range) <= 0)
	return 0;
    G_get_range_min_max (&range, &xmin, &xmax);
    *max = xmax;
    *min = xmin;
    return 1;
}

slow_range (name, mapset, min, max)
    char *name, *mapset;
    long *min, *max;
{
    FILE *fd, *popen();
    int first;
    long n;
    int ok;
    char buf[512];

    *min = *max = 0;

    printf ("one moment ...\n");
    sprintf (buf, "Gdescribe -r -1 '%s in %s'",name,mapset);
    fd = popen (buf,"r");
    if (fd == NULL)
	return 0;
    ok = 1;
    first = 1;
    while (ok && fgets(buf,sizeof(buf),fd))
    {
	ok = (sscanf (buf, "%ld", &n)==1);
	if (!ok) break;
	if (n==0) continue;
	*max = n;
	if (first) *min = n;
	first = 0;
    }
    pclose (fd);
    if (!ok)
	*min = *max = 0;
    return ok;
}
