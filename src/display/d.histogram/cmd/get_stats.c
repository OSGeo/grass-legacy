#include "options.h"
#include "dhist.h"
#include "gis.h"
#include <stdio.h> 

get_stats(mapname,mapset,dist_stats,quiet)
char *mapname;
char *mapset;
struct stat_list *dist_stats;      /* linked list of stats */
{
    char              buf[1024];    /* input buffer for reading stats */
    int               done = 0;
    char              *tempfile;     /* temp file name */
    char              *fullname;
    FILE              *fd;      /* temp file pointer */

    long int          cat;             /* a category value */
    long int          stat;            /* a category stat value */
    struct stat_node  *ptr;
    int               first;

    /* write stats to a temp file */
    tempfile = G_tempfile();
    fullname = G_fully_qualified_name(mapname,mapset);
    sprintf(buf,"r.stats -%s%s '%s' > %s\n", type==COUNT?"c":"a", quiet?"q":"", fullname, tempfile);
    if(system(buf))
    {
        fprintf (stderr, "%s: ERROR running r.stats\n", G_program_name());
        exit(1);
    }

    /* open temp file and read the stats into a linked list */
    fd = fopen(tempfile,"r");
    dist_stats->ptr = NULL;
    dist_stats->count = 0;
    dist_stats->sumstat = 0;

    first=1;
    while (!done)
    {
        if (fgets(buf,sizeof(buf),fd) != NULL)
        {
            /* WARNING!!!!!!
 * this will be very wrong if type!=COUNT
 * since the stat prodcued by r.stats will be a floating point value
 * possibly less than 1 (shapiro)
 */
            if (sscanf(buf,"%ld %ld",&cat,&stat) == 2)
            {
                if (cat==(long int)0 && nodata==NO)
                {
                }
                else
                {
                    /* count stats */
                    dist_stats->count++;

                    /* sum stats */
                    dist_stats->sumstat += stat;

                    /* a max or a min stat? */
                    if (first)
                    {
                        dist_stats->maxstat = stat;
                        dist_stats->minstat = stat;
                        dist_stats->maxcat = cat;
                        dist_stats->mincat = cat;
                        first=0;
                    }
                    if (stat>dist_stats->maxstat)
                        dist_stats->maxstat = stat;
                    if (stat<dist_stats->minstat)
                        dist_stats->minstat = stat;

                    /* a max or a min cat? */
                    if (cat>dist_stats->maxcat)
                        dist_stats->maxcat = cat;
                    if (cat<dist_stats->mincat)
                        dist_stats->mincat = cat;

                    /* put it in the list */
                    if (dist_stats->ptr == NULL)
                    {
                        /* first in list */
                        dist_stats->ptr=(struct stat_node *)
                            G_malloc(sizeof(struct stat_node));
                        dist_stats->ptr->cat = cat;
                        dist_stats->ptr->stat = stat;
                        dist_stats->ptr->next = NULL;
                        ptr = dist_stats->ptr;
                    }
                    else
                    {
                        ptr->next=(struct stat_node *)
                            G_malloc(sizeof(struct stat_node));
                        ptr->next->cat = cat;
                        ptr->next->stat = stat;
			ptr->next->next = NULL;	/* mod: shapiro */
                        ptr = ptr->next;
                    }
                }
            }
        }
        else
            done = 1;
    }
    fclose (fd);
    unlink (tempfile);
}
