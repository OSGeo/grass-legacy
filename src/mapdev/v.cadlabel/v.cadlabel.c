/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    <math.h>
#include    "gis.h"
#include    "Vect.h"
#include    "local_proto.h"

#ifndef LINE_LABELED
#define LINE_LABELED(p) (LINE_ALIVE (p) && (p)->att)      
#endif
#ifdef __CYGWIN__
#define HUGE HUGE_VAL
#endif

#define MAIN
/*#define  USAGE  "v.cadlabel lines=linefile labels=labelfile" */

/*
#define DEBUG
*/
/*  command line args */
static	char  *line_file = NULL ;
static	char  *label_file = NULL ;
static  double interval = 0.0 ;
static  int   FILL, LABEL;

static struct line_pnts Points;

int main (int argc, char **argv)
{
	struct GModule *module;
    int  ret ;
    char *line_mapset, *label_mapset;
    struct Option *file1, *file2;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Attaches labels to (binary) vector contour lines "
		"that have been imported to GRASS from DXF format.";

    file1 = G_define_option();
    file1->key			= "lines";
    file1->type			= TYPE_STRING;
    file1->required		= YES;
    file1->multiple		= NO;
    file1->gisprompt		= "old,dig,vector";
    file1->description		= "name of file containing index contour LINES";
    
    file2 = G_define_option();
    file2->key			= "labels";
    file2->type			= TYPE_STRING;
    file2->required		= YES;
    file2->multiple		= NO;
    file1->gisprompt		= "old,dig,vector";
    file2->description		= "name of file containing index contour line LABELS";

    if (G_parser (argc, argv))
	exit (-1);

    line_file = file1->answer;
    label_file = file2->answer;


    if (!*line_file  || !*label_file )
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }


/*  check args and set flags  */
	
    Points.alloc_points = 0;	/* init struct */

    FILL = 0;				/* ALWAYS FALSE (not supported) */
    LABEL = 0;
    if (interval != 0.0)
	FILL = 1;			/* not supported */
    if (label_file != NULL)
	LABEL = 1;			/* ALWAYS TRUE */

/* Show advertising */
    G_gisinit(argv[0]);
    fprintf (stdout,"\n\n   Contours:\n\n") ;

    if ((line_mapset = G_find_file2 ("dig", line_file, G_mapset())) == NULL)
    {
	char buf[150];

	sprintf (buf, "Could not find DIG file %s\n", line_file);
	G_fatal_error (buf);
    }

    if (LABEL)
	if ((label_mapset = G_find_file2 ("dig", label_file, "")) == NULL)
	{
	    char buf[150];

	    sprintf (buf, "Could not find DIG file %s\n", label_file);
	    G_fatal_error (buf);
	}
    
    setup (line_file, line_mapset, label_file, label_mapset, interval); 
    exit (0);
}

#ifdef DEBUG
#include <stdarg.h>
int debugf (char *format, ...)
{
    va_list args;
    va_start(format,args);
    vfprintf (stderr, format, args);
    va_end(args);

    return 0;
}
#endif


/*DEBUG*/ 	struct dig_head label_Head;

int setup (char *line_file, char *line_mapset,
	char *label_file, char *label_mapset, double interval)
{
	struct Map_info line_Map;
	struct Map_info label_Map;
	FILE *plus_fp;
	struct Plus_head Plus_head;

	if (!line_mapset)
	    G_fatal_error ("No mapset specified.\n");

	if (LABEL)
	{
	    if (!label_mapset)
		G_fatal_error ("No mapset specified.\n");

	    if (2 > (Vect_open_old (&label_Map, label_file, label_mapset)))
	    {
		char buf[200];
		sprintf(buf,"You must first run v.support on '%s'", label_file);
		G_fatal_error (buf);
	    }
	}


	/* unsupported function */
	if (2 > Vect__open_update_1 (&line_Map, line_file))
	{
	    char buf[200];
	    sprintf(buf,"You must first run v.support on '%s'", line_file);
	    G_fatal_error (buf);
	}

	/*
	** open up ATT file for read/write  so we can modify labels
	*/
	if ( (line_Map.att_fp = fopen(line_Map.att_file, "r+")) == NULL )
	{
	    fprintf (stderr, "Cannot open ATT file '%s' for write\n", line_Map.att_file);
	    exit (-1);
	}


	if (LABEL)
	{
	    fprintf (stdout,"Attaching Labels to contour map: \n");
	    attach_labels (&line_Map, &label_Map);
	}

/*
	if (FILL)
	{
	    fprintf (stdout,"Labelling lines using contour interval %lf\n", interval);
	    label_interval (&line_Map, interval);
	}
*/

	if (NULL == (plus_fp = fopen (line_Map.plus_file, "w")))
	{
	    fprintf (stderr, "Cant open Plus file for final write!\n");
	    exit (-1);
	}
	dig_map_to_head (&line_Map, &Plus_head);

	fclose (line_Map.att_fp);
	fclose (line_Map.dig_fp);

	/* this should be handled by Vect_close(), and someday will be */
	if (0 > dig_write_plus_file (plus_fp, &line_Map, &Plus_head))
	{
	    fprintf (stderr, "Error writing final plus file\n");
	    exit (-1);
	}

	/*  commencted out because I close  dig_fp above.  I did this so 
	**   dig_write_plus_file() could correctly  stat() the file
	**
	**  Vect_close (&line_Map);
	*/
	if (LABEL)
	    Vect_close (&label_Map);

	return(0) ;
}

/*
**  C - Contours
**  T - Text labels
*/
int attach_labels (struct Map_info *Cmap, struct Map_info *Tmap)
{
    int Tline, Cnode;
    P_LINE *TLine;
    P_NODE *CNode;
    double N_N, N_S, N_E, N_W;
    double box_height, box_width;
    int num_found, loop_cnt;
    plus_t A, B;
    double A_Dist, B_Dist;
    plus_t A_Best, B_Best;
    static struct line_pnts Points;
    double dist1, dist2;
    int Aline, Bline;
    int att, cat;
    int ret;

    for (Tline = 1 ; Tline <= Tmap->n_lines ; Tline++)
    {
	num_found = 0;
	A = B = 0;

/*DEBUG*/ debugf ("LABEL NUMBER: %d\n", Tline);
	TLine = &(Tmap->Line[Tline]);
	cat = Tmap->Att[TLine->att].cat;
	if (!LINE_LABELED (TLine))
	{
	    fprintf (stderr, "Textfile Line %d NOT labelled\n", Tline);
	    continue;
	}

	/* read in Label outline */
	if (0 > V1_read_line (Tmap, &Points, TLine->offset))
	{
	    fprintf (stderr, "Error reading LABEL dig file\n");
	    exit (-1);
	}

	A_Best = B_Best = 0;
#include "corridor.c"

	/* hack code to see something work: */
	/* always take the node found w/ the earliest past */
	for (loop_cnt = 0 ; num_found < 2 && loop_cnt < 10 ; loop_cnt ++)
	{
	    /* this line should work outside of the loop */
	    A_Best = B_Best = 0;
	    box_height = TLine->N - TLine->S;
	    box_width  = TLine->E - TLine->W;
	    N_N = TLine->N + (loop_cnt * .5 * box_height);
	    N_S = TLine->S - (loop_cnt * .5 * box_height);
	    N_E = TLine->E + (loop_cnt * .5 * box_width);
	    N_W = TLine->W - (loop_cnt * .5 * box_width);

	    for (Cnode = 1 ; Cnode <= Cmap->n_nodes ; Cnode++)
	    {
		CNode = &(Cmap->Node[Cnode]);

		/* skip if not the end of a line */
		if (CNode->n_lines != 1)
		    continue;

		/* skip if line is already labelled something else */
		if ((att = Cmap->Line[abs (CNode->lines[0])].att) &&
		    Cmap->Att[att].cat != cat)
		    continue;

		/* and skip if not in new test BBOX */
		if (CNode->y < N_S || CNode->y > N_N) continue;
		if (CNode->x < N_W || CNode->x > N_E) continue;


		/* A */
		dist1 = dist2 = HUGE;
		if (!A)
		    dist1 = dig_distance2_point_to_line (CNode->x,CNode->y,Points.x[0],Points.y[0],Points.x[1],Points.y[1]);
		if (!B)
		    dist2 = dig_distance2_point_to_line (CNode->x,CNode->y,Points.x[2],Points.y[2],Points.x[3],Points.y[3]);
		if (dist1 == dist2)	/* never gets here? */
		{
		    if (dist1 == HUGE)
			continue;
		}
		if (dist1 <= dist2)
		{
		    if (!A_Best)
		    {
			A_Best = Cnode;
			A_Dist = dist1;
		    }
		    else
		    {
			if (dist1 < A_Dist)
			{
			    A_Best = Cnode;
			    A_Dist = dist1;
			}
		    }
		}
		else 
		{
		    if (!B_Best)
		    {
			B_Best = Cnode;
			B_Dist = dist2;
		    }
		    else
		    {
			if (dist2 < B_Dist)
			{
			    B_Best = Cnode;
			    B_Dist = dist2;
			}
		    }
		}
	    }

	    if (!A)
	    {
		if (A_Best)
		{
		    A = A_Best;
		    num_found++;
		}
	    }
	    if (!B)
	    {
		if (B_Best)
		{
		    B = B_Best;
		    num_found++;
		}
	    }
	}
	if (num_found < 2)
	{
	    fprintf (stderr, "FAILED to find node for Tline %d\n", Tline);
	    continue;
	}

	/* otherwise, we should have two nodes whose lines belong
	** to this label!
	*/
	Aline = abs(Cmap->Node[A].lines[0]);
	Bline = abs(Cmap->Node[B].lines[0]);
/*DEBUG*/ debugf ("Found:  Aline %d   Bline %d\n", Aline, Bline);

	ret = label_lines (Cmap, Aline, Tmap->Att[TLine->att].cat);
	if (ret < 0)
	    fprintf (stderr, "Could not create new Label A\n");
	ret = label_lines (Cmap, Bline, Tmap->Att[TLine->att].cat);
	if (ret < 0)
	    fprintf (stderr, "Could not create new Label B\n");
    }
/*
    fclose (fpp);
*/
    return 0;
}
