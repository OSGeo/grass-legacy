#include <stdlib.h>
#include "Vect.h"
#include "local_proto.h"

/*
**  watch how routines work if A == B.  Will have to make this work
**  on a line against itself
*/

static struct line_pnts Points;
static int first = 1;


static double Threshold;
static double Threshold2;

int spagetti (
    struct Map_info *Map,
    double thresh)
{
    int A, B;
    P_LINE *Aline, *Bline;
    char *p;

    if (NULL != (p = getenv ("SPAG_THRESH")))
    {
	thresh = atof (p);
    }


/*DEBUG*/ fprintf (stderr, "Threshold = %f\n", thresh);
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

    return 0;
}


struct nearness {
    struct vertex *ptr;
    double x;	
    double y;
};

#define TO   0
#define FROM 1
#define ANY 2

struct vertex {

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

    struct vertex **link;
    int link_cnt;
    int link_alloc;
};
static int build_relations(struct vertex *, struct vertex *);
static int add_near(struct vertex *, struct vertex *, int, double, double);
static int load_line (struct Map_info *, P_LINE *, struct vertex *);
static int Link(struct vertex *, struct vertex *, int);
static int free_line(struct vertex *);
static int debug_interact(struct vertex *, struct vertex *);
static int get_window(struct vertex *, int, double *, double *, double *, double *);
static int is_a_node(struct vertex *);
static int graph_line (FILE *, struct vertex *);
static int graph_hits (FILE *, struct vertex *);

int 
process_lines (struct Map_info *Map, int A, int B)
{
    P_LINE *Aline, *Bline;
    struct vertex Ahead, Bhead;
    int cnt = 0;

    Aline = &(Map->Line[A]);
    Bline = &(Map->Line[B]);

    Ahead.link = (struct vertex **) NULL;
    Ahead.link_alloc = 0;
    Ahead.link_cnt = 0;

    Bhead.link = (struct vertex **) NULL;
    Bhead.link_alloc = 0;
    Bhead.link_cnt = 0;

    load_line (Map, Aline, &Ahead);
    load_line (Map, Bline, &Bhead);

    cnt += build_relations (&Ahead, &Bhead); /* build A side */
    cnt += build_relations (&Bhead, &Ahead); /* build B side */

    if (cnt)
	debug_interact (&Ahead, &Bhead);

    free_line (&Ahead);
    free_line (&Bhead);

    return 0;
}



/*
           Cur->link[TO]-+
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

static int build_relations (struct vertex *Ahead, struct vertex *Bhead)
{
    struct vertex *Acur, *Bcur;
    double x, y;
    int first  = 1;
    double dist;
    int cnt = 0;

    Acur = Ahead;  

    while (Acur->link[TO] != NULL)
    {
	Acur = Acur->link[TO];
	Bcur = Bhead->link[TO]; 	/* start w/ 2nd point */  /* TODO */  
			    /* watch for overstepping */
	while (Bcur->link[TO] != NULL)
	{
	    Bcur = Bcur->link[TO];

	    /* test  A pointing to Bseg */
	    x = Acur->x; y = Acur->y;
	    dist = dig_xy_distance2_point_to_line (&x, &y, 
		    Bcur->link[FROM]->x, Bcur->link[FROM]->y, Bcur->x, Bcur->y);
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
			Acur->link[FROM]->x, Acur->link[FROM]->y, Acur->x, Acur->y);
		if (Threshold2 >= dist)
		{
		    if (!(dist == 0.0 && is_a_node (Bcur)))
		    {
			add_near (Acur, Bcur, FROM, x, y);
			cnt ++;
		    }
		}

		/* test Blink[FROM] pointing to Aseg */
		x = Bcur->link[FROM]->x; y = Bcur->link[FROM]->y;
		dist = dig_xy_distance2_point_to_line (&x, &y, 
			Acur->link[FROM]->x, Acur->link[FROM]->y, Acur->x, Acur->y);
		if (Threshold2 >= dist)
		{
		    if (!(dist == 0.0 && is_a_node (Bcur->link[FROM])))
		    {
			add_near (Acur, Bcur->link[FROM], FROM, x, y);
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
static int add_near (
    struct vertex *A,
    struct vertex *B,
    int dir,
    double x,double y)
{
    char *p;
    int alloced, num;
    struct nearness *N;

/*DEBUG*/ fprintf (stderr, "Add_near \n");
    num = A->hit[dir].cnt + 1;
    alloced = A->hit[dir].alloc;
    if (!(p = dig__alloc_space (num, &alloced, MIN_HIT_ALLOC, A->hit[dir].ptr, 
	sizeof (struct nearness))))
    {
	return (dig_out_of_memory ());
    }

    A->hit[dir].ptr = (struct nearness *) p;
    A->hit[dir].alloc = alloced;

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
** list of struct vertex that represents that line, 1 elem per vertice.
*/
static int load_line (
    struct Map_info *Map,
    P_LINE *Line,
    struct vertex *Head)
{
    struct vertex *Cur, *tmp;
    int i;

    V1_read_line (Map, &Points, Line->offset);

    Cur = Head;

    /* Head does not really contain data */
    Link (Cur, NULL, FROM);

    for (i = 0 ; i < Points.n_points ; i++)
    {
	tmp = (struct vertex *) malloc (sizeof (struct vertex));
	if (tmp == NULL)
	    return (dig_out_of_memory());
	tmp->link = NULL;
	tmp->link_cnt = 0;
	tmp->link_alloc = 0;

	Link (Cur, tmp, TO);
	Link (tmp, Cur, FROM);
	Cur = tmp;

	Cur->x = Points.x[i];
	Cur->y = Points.y[i];

	Cur->hit[TO].ptr = NULL;
	Cur->hit[FROM].ptr = NULL;
	Cur->hit[TO].cnt = 0;
	Cur->hit[FROM].cnt = 0;
	Cur->hit[TO].alloc = 0;
	Cur->hit[FROM].alloc = 0;
    }

    Link (Cur, NULL, TO);

    return (0);
}

#define MIN_LINK_ALLOC 3   /* must be at least 2 */

static int Link (struct vertex *host, struct vertex *link, int dir)
{
    int alloced, num;
    char *p;

    num = host->link_cnt + 1;
    alloced = host->link_alloc;

/*DEBUG*/ fprintf (stderr, "LINK \n");
    if (!(p = dig__alloc_space (num, &alloced, MIN_LINK_ALLOC, (char *) host->link, sizeof (struct vertex *))))
    {
	return (dig_out_of_memory ());
    }
    host->link = (struct vertex **) p;
    host->link_alloc = alloced;


    if (dir == ANY)
    {
	host->link[host->link_cnt] = link;
	host->link_cnt++;
    }
    else
    {
	host->link[dir] = link;
	if (host->link_cnt <= 2)
	{
	    /* this is not good, but will work cuz will only use TO/FROM
	    ** when starting off  (i hope)
	    */
	    host->link_cnt = 2;
	}
    }

    return (0);
}

/* TODO */
static int free_line (struct vertex *Head)
{
    int i;

    while (Head->link[TO] != NULL)
	Head = Head->link[TO];
    
    while (Head->link[FROM] != NULL)
    {
	for (i = 0 ; i < 2 ; i++)
	{
	    if (Head->hit[i].alloc)
		free (Head->hit[i].ptr);
	    Head->hit[i].cnt = 0;
	    Head->hit[i].alloc = 0;
	}
	Head = Head->link[FROM];
	free (Head->link[TO]);
    }

    return 0;
}

/*
** if the bounding boxes of the two lines intersect, return 1
** else return 0
*/
int bboxes_cross (P_LINE *Aline,P_LINE *Bline)
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

static int debug_interact (struct vertex *Ahead, struct vertex *Bhead)
{
    char *p, buf[300];
    FILE *fp;
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
	if (NULL == fgets (buf,300,stdin))
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

int set_window (double N, double S, double E, double W)
{
    Sn = N, Ss = S, Se = E, Sw = W;
    reset_window ();

    return 0;
}

int reset_window (void)
{
    char buf[200];

    sprintf (buf, "Gwindow nsres=%f ewres=%f n=%f s=%f e=%f w=%f", 
	(Sn-Ss)/100., (Se-Sw)/100., Sn, Ss, Se, Sw);
/*DEBUG*/    fprintf (stderr, "%s\n", buf);
    system (buf);
    system ("Derase");  /* to reset new window */

    return 0;
}


static int get_window (
    struct vertex *ptr, int flag, double *n, double *s, double *e, double *w)
{
    /* first time, init the ranges */
    if (!flag)
    {
	*n = ptr->link[TO]->y;
	*s = ptr->link[TO]->y;
	*e = ptr->link[TO]->x;
	*w = ptr->link[TO]->x;
    }

    while (ptr->link[TO] != NULL)
    {
	ptr = ptr->link[TO];
	if (ptr->y < *s)
	    *s = ptr->y;
	if (ptr->y > *n)
	    *n = ptr->y;
	if (ptr->x < *w)
	    *w = ptr->x;
	if (ptr->x > *e)
	    *e = ptr->x;
    }

    return 0;
}

static int graph_line (FILE *fp, struct vertex *ptr)
{
    int first = 1;
    while (ptr->link[TO] != NULL)
    {
	ptr = ptr->link[TO];
	if (first)
	{
	    fprintf (fp, "move %f %f\n", ptr->x, ptr->y);
	    fprintf (fp, "icon x 2 %f %f\n", ptr->x, ptr->y);
	    first = 0;
	}
	fprintf (fp, "draw %f %f\n", ptr->x, ptr->y);
	fprintf (fp, "icon o 1 %f %f\n", ptr->x, ptr->y);
    }
    fprintf (fp, "icon x 2 %f %f\n", ptr->x, ptr->y);

    return 0;
}

static int graph_hits (FILE *fp, struct vertex *ptr)
{
    int i;

    while (ptr->link[TO] != NULL)
    {
	ptr = ptr->link[TO];
	if (ptr->hit[TO].cnt)
	{
	    for (i = 0  ; i < ptr->hit[TO].cnt ; i++)
	    {
		fprintf (fp, "move %f %f\n", ptr->x, ptr->y);
		fprintf (fp, "draw %f %f\n", ptr->hit[TO].ptr[i].x, ptr->hit[TO].ptr[i].y);
	    }
	}
    }

    return 0;
}


static int is_a_node (struct vertex *ptr)
{
    if (ptr->link[TO] == NULL)
	return 1;
    if (ptr->link[FROM] == NULL || ptr->link[FROM]->link[FROM] == NULL)
	return 1;

    return 0;
}
