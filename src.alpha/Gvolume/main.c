
/* Gvolume is a program to compute the total, and average of
   cell values within regions of a map defined by clumps or
   patches on a second map (or MASK).  It also computes
   the "volume" by multiplying the total within a clump by
   the area of each cell.  It also outputs the "centroid"
   location of each clump.  Output is to standard out.
*/

/* Programmed in Dec. 1988 by Dr. James Hinthorne, GIS Lab
   Central Washington University (revised April 1989)
*/
#include "gis.h"

main(argc, argv)
int argc; char *argv[];
{
/* variables   */
CELL *data_buf, *clump_buf;
CELL i, max;
int row, col, rows, cols;
int out_mode, use_MASK, *n, *e;
long int *count;
int fd_data, fd_clump;
char buf[200],datamap[100], clumpmap[100], site_list[100];
char *p1, *curr_mapset, *data_mapset, *clump_mapset;
double avg, vol, total_vol, east, north, *sum;
struct Cell_head window;
FILE *fd_sites, *fd_report;
extern centroids();

/* Initialize GIS */
G_gisinit(argv[0]);

sprintf(buf,"Usage: %s [-m] d=datamap [c=clumpmap] [s=site_list]\n",
			argv[0]);
if (argc < 2) G_fatal_error(buf);

/* get current window */
G_get_window(&window);

/* initialize */
*datamap=0; *clumpmap=0; *site_list=0;
out_mode = 1; /* assume full output text */

/* get arguments */
for (i=1; i<argc; i++)
	{
	switch (argv[i][0])
		{
		case 'd': if (argv[i][1] == '=') strcpy(datamap,argv[i]+2);
					break;
		case 'c': if (argv[i][1] == '=') strcpy(clumpmap,argv[i]+2);
					break;
		case 's': if (argv[i][1] == '=') strcpy(site_list,argv[i]+2);
					break;
		case '-': if (argv[i][1] == 'm') out_mode = 0; /* just data */
					break;
		default	: G_fatal_error(buf);
		}
	}
if (*datamap == 0)
	{
	fprintf(stderr,"%s\n",buf);
	G_fatal_error("No data map specified in Gvolume");
	}

/* See if MASK or a separate "clumpmap" layer is to be used--
	it must(!) be one of those two choices.
*/
use_MASK = 0;
if (*clumpmap == 0)
	{
	strcpy(clumpmap,"MASK"); use_MASK = 1;
	if(G_find_cell(clumpmap,G_mapset()) == NULL)
		G_fatal_error("No clumpmap specified and MASK not set.");
	}
curr_mapset = G_mapset();
data_mapset = G_find_cell(datamap, "");
if (!data_mapset) G_fatal_error("Can't find data map layer.");

fd_data = G_open_cell_old(datamap, data_mapset);
if (use_MASK)
	clump_mapset = curr_mapset;
else
	clump_mapset = G_find_cell(clumpmap,"");
if (!clump_mapset) G_fatal_error("Can't find clump map layer.");

fd_clump = G_open_cell_old(clumpmap, clump_mapset);	

/* initialize sites file (for centroids) if needed */
if (*site_list)
	{
	fd_sites = G_fopen_sites_new(site_list, curr_mapset);
	if (fd_sites==NULL) G_fatal_error("Can't open sites list.");
	}

/* initialize data accumulation arrays */
max = G_number_of_cats(clumpmap, clump_mapset);

sum =    (double *) G_malloc((max+1)*sizeof(double));
count =  (long int *) G_malloc((max+1)*sizeof(long int));

for (i=0; i<=max; i++)
		{sum[i]=0; count[i]=0;}

data_buf = G_allocate_cell_buf();
clump_buf= G_allocate_cell_buf();

/* get window size */
rows = window.rows;
cols = window.cols;

if (fd_data<0 || fd_clump<0)
	G_fatal_error("Data or Clump file not open");

/* now get the data -- first pass */
for (row=0; row < rows; row++)
	{
	G_get_map_row (fd_data, data_buf, row);
	G_get_map_row (fd_clump,clump_buf,row);
	for (col=0; col < cols; col++)
		{
		i = clump_buf[col];
		if (i > max)
		{sprintf(buf,"Row=%d Col=%d Cat=%d in clump map [%s]; max=%d.\n",
			row,col,i,clumpmap,max);
		strcat(buf,"Cat value > max returned by G_number_of_cats.");
		G_fatal_error(buf);
		}
		if (i < 1) continue; /* ignore zeros and negs */
		count[i]++;
		sum[i] += data_buf[col];
		}
	}
/* free some buffer space */
free(data_buf);
free(clump_buf);

/* data lists for centroids of clumps */
e = (int *) G_malloc((max+1)*sizeof(int));
n = (int *) G_malloc((max+1)*sizeof(int));

i = centroids(fd_clump, e, n, 1, max);

/* got everything, now do output */
if (*site_list)
	{
fprintf(fd_sites,"name|%s\n",site_list);
fprintf(fd_sites,"desc|from %s on map %s using clumps from %s\n",
		argv[0], datamap, clumpmap);
	}
if (out_mode)
{
printf("Gvolume report on data from %s",datamap);
printf(" using clumps on %s map\n\n", clumpmap);
printf(
" Cat    Average   Data   # Cells        Centroid             Total\n");
printf(
"Number  in clump  Total  in clump   Easting   Northing       Volume\n\n");
}
total_vol = 0.0;

for (i=1; i<=max; i++)
	{
	if (count[i])
		{
		avg = sum[i] / (double) count[i];
		vol = sum[i] * window.ew_res * window.ns_res ;
		total_vol += vol;
		east = window.west  + (e[i]+0.5)*window.ew_res;
		north= window.north - (n[i]+0.5)*window.ns_res;
		if (*site_list)
		fprintf(fd_sites,
			"%-1.2f|%-1.2f|#%5d v=%-1.2f a=%-1.2f t=%-1.0f n=%ld\n",
			   east, north,i, vol,      avg,    sum[i], count[i]);
		if (out_mode)
			printf("%5d%10.2f%10.0f %7ld  %10.2f  %10.2f %16.2f\n",
			  i, avg, sum[i],count[i],east,  north,  vol);
		else
			printf("%d:%.2f:%.0f:%ld:%.2f:%.2f:%.2f\n",
			  i, avg, sum[i],count[i],east,  north,  vol);
		}
	}
if (total_vol>0.0 && out_mode)
	printf("%58s %14.2f","Total Volume =",total_vol);
} /* end of main() */


