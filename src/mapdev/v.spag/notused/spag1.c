#include <stdlib.h>
#include "digit.h"

/*
**  watch how routines work if A == B.  Will have to make this work
**  on a line against itself
*/

static struct line_pnts Points;
static int first = 1;


static double Threshold;
static double Threshold2;

spagetti (Map, thresh)
    struct Map_info *Map;
    double thresh;
{
    int A, B;
    P_LINE *Aline, *Bline;
    char *p;
    char buf[300];

    if (NULL != (p = getenv ("SPAG_THRESH")))
    {
	thresh = atof (p);
    }


/*DEBUG*/ fprintf (stderr, "Threshold = %lf\n", thresh);
    Threshold = thresh;
    Threshold2 = thresh * thresh;

    system ("Dscreen;Dnew left 0 100 0 50;Dnew right 0 100 50 100");
    system ("Dchoose full_screen");
    system ("Derase");


    if (first)
    {
	Points.alloc_points = 0;
	first = 0;
    }
    for (A = 1 ; A <= Map->n_lines ; A++)
    {
	Aline = &(Map->Line[A]);
	if (!LINE_ALIVE (Aline))
	    continue;

	for (B = A+1 ; B <= Map->n_lines ; B++)
	{
	    Bline = &(Map->Line[B]);
	    if (!LINE_ALIVE (Bline))
		continue;

	    if (bboxes_cross (Aline, Bline))
	    {
/*DEBUG*/ fprintf (stderr, "Spag:  %d %d\n", A, B);
		process_lines (Map, A, B);
	    }
	}
    }
}


struct nearness {
    struct check_lines *ptr;
    double x;	
    double y;
};

#define TO   0
#define FROM 1

struct check_lines {

    unsigned int flags;
    int node;

    /* this is an array of pointers to near hits on other line */
    struct {
	struct nearness *ptr;
	int cnt;
	int alloc;
    } hit[2];

    double x;
    double y;

    struct check_lines *prev;
    struct check_lines *next;
};

process_lines (Map, A, B)
    struct Map_info *Map;
    int A, B;
{
    P_LINE *Aline, *Bline;
    struct check_lines Ahead, Bhead;
    int cnt = 0;

    Aline = &(Map->Line[A]);
    Bline = &(Map->Line[B]);

    load_line (Map, Aline, &Ahead);
    load_line (Map, Bline, &Bhead);

    cnt += build_relations (&Ahead, &Bhead); /* build A side */
    cnt += build_relations (&Bhead, &Ahead); /* build B side */

    if (cnt)
	debug_interact (&Ahead, &Bhead);

    free_line (&Ahead);
    free_line (&Bhead);
}



/*
           Cur->next-+
    Cur--+           |
	 |           |
  .______.___________.__________.
  |   |  |     |     |    |     |
  |   |  |     |     |    |     - Point A3
  |   |  |     |     |    - Segment A3
  |   |  |     |     - Point A2   
  |   |  |     - Segment A2    
  |   |  - Point A1    
  |   - Segment A1
  - Point A0
*/


#define RESULT_A_B   1 	/* point A1 is near segment B1(B0,B1) */
#define RESULT_B0_A  2  /* point B0 is near segment A1(A0,A1) */
#define RESULT_B1_A  4  /* point B1 is near segment A1(A0,A1) */
#define A_NODE       8
#define B_NODE      16

build_relations (Ahead, Bhead)
    struct check_lines *Ahead, *Bhead;
{
    struct check_lines *Acur, *Bcur;
    int result;
    double x, y;
    int first  = 1;
    double dist;
    int cnt = 0;

    Acur = Ahead;  

    while (Acur->next != NULL)
    {
	Acur = Acur->next;
	Bcur = Bhead->next; 	/* start w/ 2nd point */  /* TODO */  
			    /* watch for overstepping */
	while (Bcur->next != NULL)
	{
	    Bcur = Bcur->next;

	    /* test  A pointing to Bseg */
	    x = Acur->x; y = Acur->y;
	    dist = dig_xy_distance2_point_to_line (&x, &y, 
		    Bcur->prev->x, Bcur->prev->y, Bcur->x, Bcur->y);
	    if (Threshold2 >= dist)
	    {
		if (dist != 0.0 || !is_a_node (Acur))
		{
		    add_near (Acur, Bcur, TO, x, y);
		    cnt ++;
		}
	    }
		

	    if (!first)
	    {
		/* test B pointing to Aseg */
		x = Bcur->x; y = Bcur->y;
		dist = dig_xy_distance2_point_to_line (&x, &y, 
			Acur->prev->x, Acur->prev->y, Acur->x, Acur->y);
		if (Threshold2 >= dist)
		{
		    if (!(dist == 0.0 && is_a_node (Bcur)))
		    {
			add_near (Acur, Bcur, FROM, x, y);
			cnt ++;
		    }
		}

		/* test Bprev pointing to Aseg */
		x = Bcur->prev->x; y = Bcur->prev->y;
		dist = dig_xy_distance2_point_to_line (&x, &y, 
			Acur->prev->x, Acur->prev->y, Acur->x, Acur->y);
		if (Threshold2 >= dist)
		{
		    if (!(dist == 0.0 && is_a_node (Bcur->prev)))
		    {
			add_near (Acur, Bcur->prev, FROM, x, y);
			cnt ++;
		    }
		}
	    }
	}
	first = 0;
    }

    return cnt;
}

#define MIN_HIT_ALLOC 4

/*
**  add a new near hit into list for this vertice.
**  dir is the direction flag whether this is a hit to another line
**  or another line vertice hitting us
*/
add_near (A, B, dir, x, y)
    struct check_lines *A, *B;
    int dir;
    double x, y;
{
    char *p;
    int alloc;
    struct nearness *N;

    if (! A->hit[dir].cnt)
    {
	alloc = MIN_HIT_ALLOC;
	p = malloc (alloc * sizeof (struct nearness));
    }
    else
    {
	alloc = A->hit[dir].cnt+MIN_HIT_ALLOC;
	p = realloc (A->hit[dir].ptr, alloc * sizeof (struct nearness));
    }


    if (p == NULL)
	return (dig_out_of_memory ());

    A->hit[dir].ptr = (struct nearness *) p;
    A->hit[dir].alloc = alloc;

/* fill in new data */
    N = &(A->hit[dir].ptr[A->hit[dir].cnt]);
    N->ptr = B;
    N->x = x;
    N->y = y;

    A->hit[dir].cnt++; 
    return 0;
}


/* 
** given P_LINE *,  read in the line and build a doubly linked
** list of struct check_lines that represents that line, 1 elem per vertice.
*/
load_line (Map, Line, Head)
    struct Map_info *Map;
    P_LINE *Line;
    struct check_lines *Head;
{
    struct check_lines *Cur;
    int i;

    dig__Read_line (&Points, Map->digit, Line->offset);

    Cur = Head;

    /* Head does not really contain data */
    Cur->prev = NULL;

    for (i = 0 ; i < Points.n_points ; i++)
    {
	Cur->next = (struct check_lines *) malloc (sizeof (struct check_lines));
	if (Cur->next == NULL)
	    return (-1);
	Cur->next->prev = Cur;
	Cur = Cur->next;

	Cur->x = Points.x[i];
	Cur->y = Points.y[i];

	Cur->hit[TO].ptr = NULL;
	Cur->hit[FROM].ptr = NULL;
	Cur->hit[TO].cnt = 0;
	Cur->hit[FROM].cnt = 0;
	Cur->hit[TO].alloc = 0;
	Cur->hit[FROM].alloc = 0;
    }

    Cur->next = NULL;

    return (0);
}

free_line (&Head)
    struct check_lines *Head;
{
    int i;

    while (Head->next != NULL)
	Head = Head->next
    
    while (Head->prev != NULL)
    {
	for (i = 0 ; i < 2 ; i++)
	{
	    if (Head->hit[i].alloc)
		free (Head->hit[i].ptr);
	    Head->hit[i].cnt = 0;
	    Head->hit[i].alloc = 0;
	}
	Head = Head->prev;
	free (Head->next);
    }
}

/*
** if the bounding boxes of the two lines intersect, return 1
** else return 0
*/
bboxes_cross (Aline, Bline)
    P_LINE *Aline, *Bline;
{
    if (Aline->E < Bline->W)
	return 0;
    if (Aline->W > Bline->E)
	return 0;
    if (Aline->N < Bline->S)
	return 0;
    if (Aline->S > Bline->N)
	return 0;
    
    return 1;
}

debug_interact (Ahead, Bhead)
    struct check_lines *Ahead, *Bhead;
{
    char *p, buf[300];
    FILE *fp, *popen ();
    double n, s, e, w;
    double diff;

    /* init */
    get_window (Ahead, 0, &n, &s, &e, &w);
    get_window (Bhead, 1, &n, &s, &e, &w);
    diff = (n - s) * .05;
    n += diff;
    s -= diff;
    diff = (e - w) * .05;
    e += diff;
    w -= diff;

    set_window (n, s, e, w);

    while (1)
    {
	fprintf (stdout,"\nc C l r f a b A B n w q Q: ");
	if (NULL == gets (buf))
	    break;
	p = buf;
	while (*p)
	{
	    switch (*p++) {
		case 'c':  /* clear */
		    system ("Derase");
		    break;
		case 'C':  /* clear all */
		    system ("Dchoose left");
		    system ("Derase");
		    system ("Dchoose right");
		    system ("Derase");
		    break;
		case 'l':   /* chose left */
		    system ("Dchoose left");
		    break;
		case 'f':   /* chose left */
		    system ("Dchoose full_screen");
		    break;
		case 'r':   /* chose right */ 
		    system ("Dchoose right"); 
		    break;
		case 'a':   /* draw line A */
		    if (NULL == (fp = popen ("Dmapgraph", "w")))
		    {
			fprintf (stderr, "popen failed\n");
			break;
		    }
		    fprintf (fp, "color gray\n");
		    graph_line (fp, Ahead); 
		    pclose (fp);
		    break;
		case 'b':   /* draw line B */
		    if (NULL == (fp = popen ("Dmapgraph", "w")))
		    {
			fprintf (stderr, "popen failed\n");
			break;
		    }
		    fprintf (fp, "color blue\n");
		    graph_line (fp, Bhead); 
		    pclose (fp);
		    break;
		case 'A':   /* draw line A connectins */
		    if (NULL == (fp = popen ("Dmapgraph", "w")))
		    {
			fprintf (stderr, "popen failed\n");
			break;
		    }
		    fprintf (fp, "color red\n");
		    graph_hits (fp, Ahead); 
		    pclose (fp);
		    break;
		case 'B':   /* draw line B connectins */
		    if (NULL == (fp = popen ("Dmapgraph", "w")))
		    {
			fprintf (stderr, "popen failed\n");
			break;
		    }
		    fprintf (fp, "color yellow\n");
		    graph_hits (fp, Bhead); 
		    pclose (fp);
		    break;
		case 'n':  /* Dnew */
		    system ("d.window");
		    system ("Derase");
		    break;
		case 'w':  /* rewindow */
		    reset_window ();
		    break;
		case 'q': /* quit */
		    goto done;
		    break;
		case 'Q':  /* exit */
		    exit (0);
		    break;
		default: 
		    break;
	    }
	}
    }
done:

    return 0;
}

static double Sn, Ss, Se, Sw;

set_window (N, S, E, W)
    double N, S, E, W;
{
    Sn = N, Ss = S, Se = E, Sw = W;
    reset_window ();
}

reset_window ()
{
    char buf[200];

    sprintf (buf, "Gwindow nsres=%lf ewres=%lf n=%lf s=%lf e=%lf w=%lf", 
	(Sn-Ss)/100., (Se-Sw)/100., Sn, Ss, Se, Sw);
/*DEBUG*/    fprintf (stderr, "%s\n", buf);
    system (buf);
    system ("Derase");  /* to reset new window */
}


get_window (ptr, flag, n, s, e, w)
    struct check_lines *ptr;
    int flag;
    double *n, *s, *e, *w;
{
    /* first time, init the ranges */
    if (!flag)
    {
	*n = ptr->next->y;
	*s = ptr->next->y;
	*e = ptr->next->x;
	*w = ptr->next->x;
    }

    while (ptr->next != NULL)
    {
	ptr = ptr->next;
	if (ptr->y < *s)
	    *s = ptr->y;
	if (ptr->y > *n)
	    *n = ptr->y;
	if (ptr->x < *w)
	    *w = ptr->x;
	if (ptr->x > *e)
	    *e = ptr->x;
    }
}

graph_line (fp, ptr)
    FILE *fp;
    struct check_lines *ptr;
{
    int first = 1;
    while (ptr->next != NULL)
    {
	ptr = ptr->next;
	if (first)
	{
	    fprintf (fp, "move %lf %lf\n", ptr->x, ptr->y);
	    fprintf (fp, "icon x 2 %lf %lf\n", ptr->x, ptr->y);
	    first = 0;
	}
	fprintf (fp, "draw %lf %lf\n", ptr->x, ptr->y);
	fprintf (fp, "icon o 1 %lf %lf\n", ptr->x, ptr->y);
    }
    fprintf (fp, "icon x 2 %lf %lf\n", ptr->x, ptr->y);
}

graph_hits (fp, ptr)
    struct check_lines *ptr;
    FILE *fp;
{
    int first = 1;
    int i;

    while (ptr->next != NULL)
    {
	ptr = ptr->next;
	if (ptr->hit[TO].cnt)
	{
	    for (i = 0  ; i < ptr->hit[TO].cnt ; i++)
	    {
		fprintf (fp, "move %lf %lf\n", ptr->x, ptr->y);
		fprintf (fp, "draw %lf %lf\n", ptr->hit[TO].ptr[i].x, ptr->hit[TO].ptr[i].y);
	    }
	}
    }
}


is_a_node (ptr)
    struct check_lines *ptr;
{
    if (ptr->next == NULL)
	return 1;
    if (ptr->prev == NULL || ptr->prev->prev == NULL)
	return 1;

    return 0;
}
