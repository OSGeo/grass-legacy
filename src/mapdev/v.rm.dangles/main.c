/*
 *  v.rm.dangles
 *  J.Soimasuo 15.9.1994 
 *  University of Joensuu, Faculty of Forestry, Finland
 */

#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

static struct line_pnts *Points, *OPoints;
static int *line_done;
static double *x,*y;
char *mapset;

double line_length();

int main (argc, argv)
    int argc;
    char *argv[];
{
    register int ret, error;
    char in_name[1024], out_name[1024];
    struct Option *opt1, *opt2, *opt3;
    struct Map_info In, Out;
    int level;
    double maxl;

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
    opt2->description= "Name of the output vector file" ;

    opt3 = G_define_option() ;
    opt3->key        = "maxlength" ;
    opt3->type       = TYPE_DOUBLE ;
    opt3->answer     = "-1";
    opt3->required   = NO ;
    opt3->description= "Maximum length of a dangle to be deleted" ;
   

    Points = Vect_new_line_struct ();
    OPoints = Vect_new_line_struct ();
    line_done = (int *) G_malloc (sizeof (int));
    x = (double *) G_malloc (sizeof(double));
    y = (double *) G_malloc (sizeof(double));

    setbuf (stdout, NULL);
    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
        exit(-1);
   
    error = 0; 

    /* Open Input and output vector files */

    strcpy (in_name, opt1->answer);
    strcpy (out_name, opt2->answer);

    sscanf(opt3->answer,"%lf",&maxl);

    if (error)
	exit (-1);

/* Copy header stuff */
    mapset = G_find_file2 ("dig", in_name, "");
    level = Vect_open_old (&In, in_name, mapset);
    if (0 > level)
	G_fatal_error ("Cannot open vector file");
    if (2 > level)
	G_fatal_error ("Must first run v.support on vector file");

    if (0 > Vect_open_new (&Out, out_name))
	G_fatal_error ("Could not create vector file");


    Vect_copy_head_data (&(In.head), &(Out.head));

    ret = dangles_out(&In, &Out, maxl);

    if (ret < 0)
       fprintf (stderr, "Error reading file '%s'.  Some data may not be correct\n");

    Vect_close (&Out);
    Vect_close (&In);
    exit (0);
}

int dangles_out(In, Out, maxl)
struct Map_info *In, *Out;
double maxl;
{
    int line, type;
    int nlines; 
    register int N1, N2, pnts;
    struct line_pnts *points;
    double length;
    points=Vect_new_line_struct();

    nlines=V2_num_lines(In);
    	for(line=1;line<=nlines;line++)
	{

		N1 = In->Node[In->Line[line].N1].n_lines;
        	N2 = In->Node[In->Line[line].N2].n_lines;
		V2_read_line(In,points,line);

		if(((N1>1) && (N2>1)) || ((maxl> 0) && (line_length(points) > maxl)) )
		{
			type=In->Line[line].type;
			Vect_write_line(Out,type,points);
		}
	}
	return(1);
}


double line_length(Points)
struct line_pnts *Points;
{
register int np, i;
double x1,x2,y1,y2;
double length=0;
np=Points->n_points;
for (i=0;i<(np-1);i++)
	{
	x1=*(Points->x+i);
	y1=*(Points->y+i);
	x2=*(Points->x+i+1);
	y2=*(Points->y+i+1);
	length=length+sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
	}
return(length);
}
