#define DEBUG

#include "options.h"
#include "dhist.h"
#include "gis.h"
#include <values.h> 
#include <stdio.h> 

get_stats(mapname,dist_stats)
char *mapname;
struct stat_list *dist_stats;      /* linked list of stats */
{
char              *inbuf[1024];    /* input buffer for reading stats */
int               done = 0;
char              stats_cmd[512];  /* string for Gstats command */
char              *temp_fname;     /* temp file name */
FILE              *temp_file;      /* temp file pointer */

long int          cat;             /* a category value */
long int          stat;            /* a category stat value */
struct stat_node  *ptr;
int               first;

/* write stats to a temp file */
temp_fname = G_tempfile();
if (type==COUNT)
   sprintf(stats_cmd,"Gstats -c %s > %s\n",mapname,temp_fname);
else
   sprintf(stats_cmd,"Gstats %s > %s\n",mapname,temp_fname);
system(stats_cmd);

/* open temp file and read the stats into a linked list */
temp_file = fopen(temp_fname,"r");
dist_stats->ptr = NULL;
dist_stats->count = 0;
dist_stats->sumstat = 0;

first=1;
while (!done)
   {
	if (fgets(inbuf,1024,temp_file) != NULL)
	{
		if (sscanf(inbuf,"%ld:%ld",&cat,&stat) == 2)
		{
			if (cat==(long int)0 && nodata==NO) 
                                  {}
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
				ptr = ptr->next;
			}
                               }
		}
	}
	else
		done = 1;
   }
}
