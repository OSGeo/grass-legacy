/*
**  Gtrim
**
*/

/*
**  Mike BABA 
**  DBA Systems
**  Fairfax, VA.
*/

#include <stdio.h>
#include "gis.h"
#include "digit.h"
#include "dig_head.h"

#define THIN_LESS_THAN 10.0

static struct line_pnts *Points;
static struct Map_info  *map;
static int *line_done;
static double *x,*y;
static int npoints;
char *mapset;

main (argc, argv)
    int argc;
    char *argv[];
{
    register int i, ret, error;
    char path[1024], filename[1024], vectname[1024], trimname[1024];
    FILE *In, *Out, *fopen ();
    struct head Head;
    double trim_factor;

    map = (struct Map_info *) G_malloc (sizeof (struct Map_info));
    Points = (struct line_pnts *) G_malloc (sizeof (struct line_pnts));
    line_done = (int *) G_malloc (sizeof (int));
    x = (double *) G_malloc (sizeof(double));
    y = (double *) G_malloc (sizeof(double));

    setbuf (stdout, NULL);
    G_gisinit (argv[0]);
    if (argc < 3)  {
	fprintf (stderr, "Usage: %s vect_file trim_file [trim_factor] \n", argv[0]),
	exit (-1);
    }
   
    error = 0; 
    /* Open Input and output vector files */
    strcpy (vectname, argv[1]);
    if ( (mapset = G_find_vector (vectname, "")) == NULL)
    {
       fprintf (stderr, "Cannot find '%s'\n", vectname);
       error++;
    }

    In = G_fopen_vector_old(vectname,mapset);
    if (In == NULL)
    {
       fprintf (stderr, "Cannot open '%s'\n", vectname);
       error++;
    }

    strcpy (trimname, argv[2]);
    Out = G_fopen_vector_new(trimname,G_mapset());
    if (Out == NULL)
    {
       fprintf (stderr, "Cannot open '%s'\n", trimname);
       error++;
    }

    /* determine trim factor */
    if (argc == 4) {
       if (! (sscanf (argv[3], "%lf", &trim_factor) == 1))  {
	  fprintf (stderr, "Usage: %s vect_file trim_file [trim_factor] \n", argv[0]);
	  exit (-1);
       }
    }
    else trim_factor = THIN_LESS_THAN ;

    if (error)
	exit (-1);

    /* display what we have */
    fprintf (stdout, "\n");
    fprintf (stdout, "    Trimming file %s trim factor = %lf \n", argv[1], trim_factor);
    ret = trim(In, Out, vectname, trim_factor);
    if (ret < 0)
       fprintf (stderr, "Error reading file '%s'.  Some data may not be correct\n");
    fclose (In);
    fclose (Out);

    exit (0);
}

trim (In, Out, vectname, trim_factor)
    FILE *In, *Out;
    char *vectname;
    double trim_factor;
{
    register int itype;
    long offset, ftell ();
    struct head local_head;
    double nx, ny;
    int np, nlines, nnodes, npoints, indx, line_indx, i, j, k, l;
    int lines_per_node, snap_to_node, snap_from_line, short_line;
    int next_node, lines_per_next_node, next_line_indx, last_point;

/* Copy header stuff */
    dig_P_init (vectname, mapset, map);
    dig_read_head_binary(In,&local_head);
    strcpy (local_head.map_name, "Output from Vtrim");
    strcpy (local_head.your_name, G_whoami ());
    dig_write_head_binary (Out, &local_head);

/* realloc line_done array */
    nlines = dig_P_num_lines(map);
    line_done = (int *) G_realloc (line_done, (nlines+1)*sizeof(int));
    for (i = 0; i <= nlines; i++)
        line_done[i] = 0;

/* remove spurs that start at node with a single line */
    nnodes = map->n_nodes;
    for (i = 1; i <= nnodes; i++)
    {   
        lines_per_node = map->Node[i].n_lines;
        if (lines_per_node !=1) continue;
   
        nx = map->Node[i].x;
        ny = map->Node[i].y;
        line_indx = map->Node[i].lines[0];
        if (line_done[ABS(line_indx)] > 0) continue;
 
        npoints = 0;
        itype = dig_P_read_line (map, ABS(line_indx), &Points);
        line_done[ABS(line_indx)] = 1;
        last_point = npoints;
        npoints += Points->n_points;
        x = (double *) G_realloc (x, npoints * sizeof(double));
        y = (double *) G_realloc (y, npoints * sizeof(double));
        if (line_indx > 0)
             for (k = 0; k < Points->n_points; k++)
             {
                x[k] = Points->x[k];  
                y[k] = Points->y[k];
             }
        else
             for (k = 0; k < Points->n_points; k++)
             {
                x[k] = Points->x[Points->n_points - k - 1];  
                y[k] = Points->y[Points->n_points - k - 1];
             }
        for ( ; ; )
	{
          next_node = line_indx < 0 ? map->Line[ABS(line_indx)].N1 : map->Line[line_indx].N2; 
          lines_per_next_node = map->Node[next_node].n_lines;

          if (lines_per_next_node != 2) break;
           
          next_line_indx = map->Node[next_node].lines[0];
          if ( ABS(next_line_indx) == ABS(line_indx))
             next_line_indx = map->Node[next_node].lines[1];
          if(line_done[ABS(next_line_indx)] > 0) break;
          line_done[ABS(next_line_indx)] = 1;
  
          itype = dig_P_read_line (map, ABS(next_line_indx), &Points);

          last_point = npoints;
          npoints += (Points->n_points - 1);
          x = (double *) G_realloc (x, npoints * sizeof(double));
          y = (double *) G_realloc (y, npoints * sizeof(double));
          if (next_line_indx > 0)
             for (l = last_point, k = 1; k < Points->n_points; k++, l++)
	        {
                   x[l] = Points->x[k];
                   y[l] = Points->y[k];
                }
          else
             for (l = last_point, k = 1; k < Points->n_points; k++, l++)
	        {
                   x[l] = Points->x[Points->n_points - k - 1];
                   y[l] = Points->y[Points->n_points - k - 1];
                }   
          line_indx = next_line_indx;
	}
        if ( (((nx - map->Node[next_node].x)*(nx - map->Node[next_node].x)) +                ((ny - map->Node[next_node].y)*(ny - map->Node[next_node].y)))                 > (trim_factor * trim_factor) )
           dig_Write_line (Out, (char) itype, x, y, npoints);
        else dig_node_del_line (&(map->Node[next_node]), line_indx);
    }     

/* snap junctions together */
    for (i = 1; i <= nnodes; i++)
    {   
        nx = map->Node[i].x;
        ny = map->Node[i].y;
        lines_per_node = map->Node[i].n_lines;

        if (lines_per_node != 3) continue;
        for (j = 0; j < lines_per_node; j++)
	{
          npoints = 0;
          line_indx = map->Node[i].lines[j];
          if (line_done[ABS(line_indx)] > 0) continue;
          
          itype = dig_P_read_line (map, ABS(line_indx), &Points);
          line_done[ABS(line_indx)] = 1;
          last_point = npoints;
          npoints += Points->n_points;
          x = (double *) G_realloc (x, npoints * sizeof(double));
          y = (double *) G_realloc (y, npoints * sizeof(double));
          if (line_indx > 0)
             for (k = 0; k < Points->n_points; k++)
             {
                x[k] = Points->x[k];  
                y[k] = Points->y[k];
             }
          else
             for (k = 0; k < Points->n_points; k++)
             {
                x[k] = Points->x[Points->n_points - k - 1];  
                y[k] = Points->y[Points->n_points - k - 1];
             }   
          for ( ; ; )
	  {
             next_node = line_indx < 0 ? map->Line[ABS(line_indx)].N1 : map->Line[line_indx].N2; 
             lines_per_next_node = map->Node[next_node].n_lines;

             if (lines_per_next_node != 2) break;
           
             next_line_indx = map->Node[next_node].lines[0];
             if ( ABS(next_line_indx) == ABS(line_indx))
                next_line_indx = map->Node[next_node].lines[1];

             if (line_done[ABS(next_line_indx)] > 0) break;

             itype = dig_P_read_line (map, ABS(next_line_indx), &Points);
             line_done[ABS(next_line_indx)] = 1;

             last_point = npoints;
             npoints += (Points->n_points - 1);
             x = (double *) G_realloc (x, npoints * sizeof(double));
             y = (double *) G_realloc (y, npoints * sizeof(double));
             if (next_line_indx > 0)
                for (l = last_point, k = 1; k < Points->n_points; k++, l++)
	        {
                   x[l] = Points->x[k];
                   y[l] = Points->y[k];
                }
             else
                for (l = last_point, k = 1; k < Points->n_points; k++, l++)
	        {
                   x[l] = Points->x[Points->n_points - k - 1];
                   y[l] = Points->y[Points->n_points - k - 1];
                }   
             line_indx = next_line_indx;
	  }
          dig_Write_line (Out, (char) itype, x, y, npoints);
	}
    }
    dig_P_fini(map);
    return (0);
}





