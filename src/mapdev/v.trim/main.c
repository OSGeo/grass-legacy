/*
**  Mike BABA 
**  DBA Systems
**  Fairfax, VA.
*/

#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

static struct line_pnts *OPoints;
static struct line_pnts *Points;
static int *line_done;
static double *x,*y;
char *mapset;

main (argc, argv)
    int argc;
    char *argv[];
{
    register int ret, error;
    char vectname[1024], trimname[1024];
    double trim_factor;
    struct Option *opt1, *opt2, *opt3;
    struct Map_info In, Out;
    int level;

    opt1 = G_define_option() ;
    opt1->key        = "input" ;
    opt1->type       = TYPE_STRING ;
    opt1->gisprompt  = "old,dig,Vector";
    opt1->required   = YES ;
    opt1->description= "Name of existing vector file" ;

    opt2 = G_define_option() ;
    opt2->key        = "output" ;
    opt2->type       = TYPE_STRING ;
    opt2->gisprompt  = "new,dig,Vector";
    opt2->required   = YES ;
    opt2->description= "Name of the new trimmed vector file" ;

    opt3 = G_define_option() ;
    opt3->key        = "factor" ;
    opt3->type       = TYPE_DOUBLE ;
    opt3->answer     = "10.0" ;
    opt3->options    = "0-100" ;
    opt3->required   = NO ;
    opt3->description= "Set the trim factor" ;

    Points = Vect_new_line_struct ();
    OPoints = Vect_new_line_struct ();
    line_done = (int *) G_malloc (sizeof (int));
    x = (double *) G_malloc (sizeof(double));
    y = (double *) G_malloc (sizeof(double));

    setbuf (stdout, NULL);
    G_gisinit (argv[0]);

    /*
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s vect_file= trim_file= [factor]=\n", argv[0]);
        exit(-1);
    }
    */


    if (G_parser(argc, argv))
        exit(-1);

   
    error = 0; 

    /* Open Input and output vector files */

    strcpy (vectname, opt1->answer);
    strcpy (trimname, opt2->answer);

    /* determine trim factor */
   sscanf (opt3->answer, "%lf", &trim_factor) ;

    if (error)
	exit (-1);

/* Copy header stuff */
    mapset = G_find_file2 ("dig", vectname, "");
    level = Vect_open_old (&In, vectname, mapset);
    if (0 > level)
	G_fatal_error ("Can't open vector file");
    if (2 > level)
	G_fatal_error ("Must first run v.support on vector file");

    if (0 > Vect_open_new (&Out, trimname))
	G_fatal_error ("Could not creat vector file");


    Vect_copy_head_data (&(In.head), &(Out.head));
    /*
    strcpy (Out.head.map_name, "Output from Vtrim");
    strcpy (Out.head.your_name, G_whoami ());
    */

    /* display what we have */
    fprintf (stdout, "\n");
    fprintf (stdout, "    Trimming file %s trim factor = %lf \n", opt1->answer, trim_factor);
    ret = trim(&In, &Out, trim_factor);
    Vect_close (&Out);
    Vect_close (&In);
    exit (0);
}

trim (In, Out, trim_factor)
    struct Map_info *In, *Out;
    double trim_factor;
{

    register int itype;
    double nx, ny, lastx, lasty, length;
    double dist();
    int nlines, nnodes, npoints, line_indx, i, k, l;
    int lines_per_node;
    int next_node, lines_per_next_node, next_line_indx, last_point;

/* realloc line_done array */
    nlines = V2_num_lines(In);
    line_done = (int *) G_realloc (line_done, (nlines+1)*sizeof(int));
    for (i = 0; i <= nlines; i++)
        line_done[i] = 0;
    nnodes = In->n_nodes;

    /* remove spurs that start at node with a single line */
    for (i = 1; i <= nnodes; i++)
    {   
        lines_per_node = In->Node[i].n_lines;
        if (lines_per_node !=1) continue;
   
        nx = In->Node[i].x;
        ny = In->Node[i].y;
        lastx = In->Node[i].x;
        lasty = In->Node[i].y;
	length = 0.;
        line_indx = In->Node[i].lines[0];
        if (line_done[abs(line_indx)] > 0) continue;
 
        npoints = 0;
        itype = V2_read_line (In, Points, abs(line_indx));
        line_done[abs(line_indx)] = 1;
        last_point = npoints;
        npoints += Points->n_points;
        x = (double *) G_realloc (x, npoints * sizeof(double));
        y = (double *) G_realloc (y, npoints * sizeof(double));
        if (line_indx > 0)
             for (k = 0; k < Points->n_points; k++)
             {
                x[k] = Points->x[k];  
                y[k] = Points->y[k];
		length = length + dist(lastx, lasty, x[k], y[k]);
		lastx= x[k]; lasty= y[k];
             }
        else
             for (k = 0; k < Points->n_points; k++)
             {
                x[k] = Points->x[Points->n_points - k - 1];  
                y[k] = Points->y[Points->n_points - k - 1];
		length = length + dist(lastx, lasty, x[k], y[k]);
		lastx= x[k]; lasty= y[k];
             }
        for ( ; ; )
	{
          next_node = line_indx < 0 ? In->Line[abs(line_indx)].N1 : In->Line[line_indx].N2; 
          length = length + dist(lastx, lasty, In->Node[next_node].x, In->Node[next_node].y);
	  lastx= In->Node[next_node].x; lasty= In->Node[next_node].y;
          lines_per_next_node = In->Node[next_node].n_lines;

          if (lines_per_next_node != 2) break;
           
          next_line_indx = In->Node[next_node].lines[0];
          if ( abs(next_line_indx) == abs(line_indx))
             next_line_indx = In->Node[next_node].lines[1];
          if(line_done[abs(next_line_indx)] > 0) break;

          line_done[abs(next_line_indx)] = 1;
  
          itype = V2_read_line (In, Points, abs(next_line_indx));

          last_point = npoints;
          npoints += (Points->n_points - 1);
          x = (double *) G_realloc (x, npoints * sizeof(double));
          y = (double *) G_realloc (y, npoints * sizeof(double));
          if (next_line_indx > 0)
             for (l = last_point, k = 1; k < Points->n_points; k++, l++)
	        {
                   x[l] = Points->x[k];
                   y[l] = Points->y[k];
		   length = length + dist(lastx, lasty, x[k], y[k]);
		   lastx= x[k]; lasty= y[k];
                }
          else
             for (l = last_point, k = 1; k < Points->n_points; k++, l++)
	        {
                   x[l] = Points->x[Points->n_points - k - 1];
                   y[l] = Points->y[Points->n_points - k - 1];
		   length = length + dist(lastx, lasty, x[k], y[k]);
		   lastx= x[k]; lasty= y[k];
                }   
          line_indx = next_line_indx;
	} /* for loop */
	/*  debugging distance 
        printf("%f  %f\n", length, dist(nx,ny, (double)In->Node[next_node].x, (double)In->Node[next_node].y));
	*/
        if (length > trim_factor)
	{
	   Vect_copy_xy_to_pnts (OPoints, x, y, npoints);
           Vect_write_line (Out, itype, OPoints);
	}
        else 
	   dig_node_del_line (&(In->Node[next_node]), line_indx);
    }     
    /* now process the rest of lines traversing them with depth first search */
    for (i = 1; i <= nnodes; i++)
    {   
        lines_per_node = In->Node[i].n_lines;
	if(lines_per_node > 1)
	/* traverse with DFS starting with node i */
	   traverse_from_node(i, In, Out, trim_factor);
    }

    return (0);
}
   
    traverse_from_node(i, In, Out, trim_factor)
        int i;
        struct Map_info *In, *Out;
        double trim_factor;
    {
        register int itype;
        int branch, npoints, line_indx, j, k, l;
        int lines_per_node;
        int next_node, lines_per_next_node, next_line_indx, last_point;

        lines_per_node = In->Node[i].n_lines;

	for (j=0;j< lines_per_node;j++)
	{
            line_indx = In->Node[i].lines[j];
            if (line_done[abs(line_indx)] > 0) continue;
            npoints = 0;
            itype = V2_read_line (In, Points, abs(line_indx));
            line_done[abs(line_indx)] = 1;
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
              next_node = line_indx < 0 ? In->Line[abs(line_indx)].N1 : In->Line[line_indx].N2; 
              lines_per_next_node = In->Node[next_node].n_lines;

              if (lines_per_next_node != 2)
	      {
	           branch = 1;
	           break;
              }
           
              next_line_indx = In->Node[next_node].lines[0];
              if ( abs(next_line_indx) == abs(line_indx))
                    next_line_indx = In->Node[next_node].lines[1];
              if(line_done[abs(next_line_indx)] > 0) 
	      {
	          branch = 0;
	          break;
              }
              line_done[abs(next_line_indx)] = 1;
  
              itype = V2_read_line (In, Points, abs(next_line_indx));

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
	     } /* for loop */

	     Vect_copy_xy_to_pnts (OPoints, x, y, npoints);
             Vect_write_line (Out, itype, OPoints);
	     
	     if(branch)
	         traverse_from_node(next_node, In, Out, trim_factor);
		 /* recurse */
	} /* for loop */
	return 0;
    }     

double dist(x1, y1, x2, y2)
  double x1, y1, x2, y2;
{
   extern double sqrt();
   return sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2));
}
