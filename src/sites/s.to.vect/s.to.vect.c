#include <stdio.h>
#include "gis.h"
#include "digit.h"
#include "dig_head.h"

#define MAIN
#define  USAGE  "sites.to.vect sitefile=input digfile=output\n"

#ifdef OLD_PARSE
/*  command line args */
static	char  *site_name = NULL ;
static	char  *dig_name = NULL ;

static  int  load_args() ;

struct Command_keys vars[] = {
 { "sitefile", 1 },
 { "digfile", 2 },
 { "input", 1 },
 { "output", 2 },
 { NULL,     0 }
};

#endif /*OLD_PARSE*/
struct site_list {
	double east;
	double north;
	long att;
	char *att_str;
};

static char *sites_fname = NULL;
static char *dig_fname = NULL;

main(argc,argv)
int argc;
char *argv[];
{
    int ret;
	char *progname, *mapset;
    char errstr[50];
	long i, j, k, l;

	struct Option *in_opt, *out_opt;


    G_gisinit(argv[0]) ;
	progname = G_program_name();

	in_opt = G_define_option();
	in_opt->key			= "input";
	in_opt->type		= TYPE_STRING;
	in_opt->required	= YES;
	in_opt->description	= "input sites filename";;

	out_opt = G_define_option();
	out_opt->key			= "output";
	out_opt->type		= TYPE_STRING;
	out_opt->required	= YES;
	out_opt->description	= "output vector filename";;

   
    if (G_parser (argc, argv))
		exit (-1);


	sites_fname = in_opt->answer;
	dig_fname = out_opt->answer;

    /*verify that required filenames are there*/
    if (!*sites_fname  || !*dig_fname)
    {
        fprintf (stderr, "\n\n%s: Command line error: missing sites input name or vector output name.\n\n", progname);
        G_usage();
        exit (-1);
    }



/*    fprintf (stderr, "infile = %s  outfile = %s\n", sites_fname, dig_fname);*/	


    if ((mapset = G_find_file2 ("site_lists", sites_fname, "")) == NULL)
	{
		sprintf (errstr, "Could not find SITE file '%s'\n", sites_fname);
		G_fatal_error (errstr);
    }  

	/* make sure dest file doesn't already exist */
	if (G_find_file2 ("dig", dig_fname, G_mapset()) != NULL)
	{
		fprintf (stderr, "file %s already exists. exiting.\n", dig_fname);
		exit(-1);
	}

	sites_to_vect (sites_fname, mapset, dig_fname);

    fprintf (stderr, "\ns.to.vect finished.\n");
    fprintf(stderr, "\n\nBefore vector file <%s> can be used in the 'digit' program:\nRun the program support.vect to build the  needed support files.\n",
        dig_fname) ;


	exit(0);
}

/*
struct Map_info Map;
struct dig_head Head;
*/

sites_to_vect(site_name, mapset, dig_name)
    char *site_name, *mapset, *dig_name;
{

	FILE *Dig_Out, *Att_Out, *Cat_Out;
	FILE *In;
	struct site_list *list;
	char *temp;

	struct dig_head Head;
	double east, north;
	double xarray[2], yarray[2];
	double x_border, y_border;
	char *desc;
	char buf[1024];
	char attfile_str[50];
	long att_num;
	int i, n;
    int day, yr;
    char date[25],  mon[4];
	int some_desc_ok, all_desc_ok;

	i = 0;

	if (!mapset) 
		G_fatal_error ("No mapset specified.\n");

	if (NULL == (In = G_fopen_old ("site_lists", site_name, mapset)))
	{
	    fprintf (stderr, "Cannot open site_list file for input.\n");
	    exit (-1);
	}

	if (NULL == (Dig_Out = G_fopen_new ("dig", dig_name))) 
	{
		fprintf (stderr, "Could not create target digit file\n", dig_name);
	    exit (-1);
	}

	if (NULL == (Att_Out = G_fopen_new ("dig_att", dig_name))) 
	{
		fprintf (stderr, "Could not create target digit att file\n", dig_name);
	    exit (-1);
	}

	if (NULL == (Cat_Out = G_fopen_new ("dig_cat", dig_name))) 
	{
		fprintf (stderr, "Could not create target digit cat file\n", dig_name);
	    exit (-1);
	}

    G_strncpy(date, G_date(), 24);
    sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
    if (yr < 2000) yr = yr - 1900;
    else yr = yr - 2000;
    sprintf(date,"%s %d %d",mon,day,yr);

	/*write head place holder*/
	strcpy (Head.organization, " ");
	strcpy (Head.date, date);
	strcpy (Head.your_name, G_whoami());
	strcpy (Head.map_name, dig_name);
	strcpy (Head.source_date, "");
	strcpy (Head.line_3, "");
	Head.orig_scale = 24000;
	Head.plani_zone = 13;
	Head.digit_thresh = 0.0;
	Head.map_thresh = 0.0;
    dig_write_head_binary (Dig_Out, &Head);

	list = NULL;
	temp = NULL;

    n = 0;		
	all_desc_ok = 1;
	some_desc_ok = 0;


	for (n=0; G_get_site (In, &east, &north, &desc) > 0; n++) 
	{
		if (n % 50 == 0)
		{
			/*DEBUG*/
			printf ("count = %d.  allocating more memory.\n", n);
			/* */
			list = (struct site_list *) G_realloc (list, (n + 50)* sizeof (struct site_list));
		}
		list[n].east = east;
		list[n].north = north;
		if (*desc == '#') desc++;
		temp = G_malloc ( strlen(desc)+1);
		*temp=0;
		att_num = 1;
		if ((sscanf (desc, "%ld%[^\n]", &att_num, temp)) < 1)
			all_desc_ok = 0;
		else
			some_desc_ok++;

		list[n].att = att_num;
		list[n].att_str = temp;

		/*DEBUG
		if (i++ > 10)
		   break;
		printf ("e %lf   n %lf  : %s\n", east, north, desc);
			**/
	}

	if (!all_desc_ok && some_desc_ok)
	{
		 fprintf (stderr, "Some site descr. not in proper format.\n");
		 fprintf (stderr, "Must be '#number description'.\n");
		 fprintf (stderr, "Setting all attributes to '1' in vector file.\n");
	}

	if (n)
	{
		Head.W = Head.E = xarray[0] = xarray[1] = list[0].east;
		Head.N = Head.S = yarray[0] = yarray[1] = list[0].north;

		for (i = 0; i < n; i++)
		{
			xarray[0] = xarray[1] = list[i].east;
			yarray[0] = yarray[1] = list[i].north;
			Head.N = GREATER (Head.N, yarray[0]);
			Head.E = GREATER (Head.E, xarray[0]);
			Head.S = LESSER (Head.S, yarray[0]);
			Head.W = LESSER (Head.W, xarray[0]);
			dig_Write_line (Dig_Out, DOT, xarray, yarray, 2);

			/*this doesn't look right: printing 1 if all are ok?*/
			if (all_desc_ok)
			{
			     fprintf (Att_Out, "P  %9.2f %9.2f %8ld\n", 
				   list[i].east, list[i].north, 1);
			}
			else
			{
			     fprintf (Att_Out, "P  %9.2f %9.2f %8ld\n", 
				   list[i].east, list[i].north, list[i].att);
			}
		}
		/*now write out real header, first setting borders 1% wider*/
		x_border = (Head.E - Head.W) * .01;
		y_border = (Head.N - Head.S) * .01;
		Head.E = Head.E + x_border;
		Head.W = Head.W - x_border;
		Head.N = Head.N + y_border;
		Head.S = Head.S - y_border;
		dig_write_head_binary (Dig_Out, &Head);
	}
	else 
	{
		 fprintf (stderr, "No site data in '%s' file.\n", site_name);
		 fclose (Dig_Out);
		 fclose (Att_Out);
		 fclose (Cat_Out);
		 G_remove ("dig", dig_name);
		 G_remove ("dig_att", dig_name);
		 G_remove ("dig_cat", dig_name);
		 exit(-1);
	}

	
	free (list);

	fclose (In);
	fclose (Dig_Out);
	fclose (Att_Out);
	fclose (Cat_Out);
	return (0);

}
#ifdef FOOLISH

/* Cell-file area extraction */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* December, 1987 */

/* 
** last modified by Dave Gerdes 8 1988
**  cleaned up the options and created dig and dig_att files
*/

#include <stdio.h>
#include "gis.h"
#include "extr_areas.h"


main(argc,argv)
int argc;
char *argv[];
{
  char *input, *output;

  if (syntax(argc,argv,&input,&output))
  {
    fprintf(stderr,"Usage:  %s cell_file digit_file\n",argv[0]);
    exit(-1);
  }
#ifdef DEBUG
  freopen("debug.out","w",stdout);
  setbuf(stdout,0);
#endif
  G_gisinit(argv[0]) ;
  fprintf(stdout,"Opening files\n");
  open_file(input,output);
  fprintf(stdout,"Performing extraction\n");
  extract_areas();
  fprintf(stdout,"Consolidating area information\n");
  re_map_areas();
  fprintf(stdout,"Closing files\n");
  close_file();
  exit(0);
}


#endif /*FOOLISH*/
#ifdef FOOLISH

/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"


#define MAIN
#define  USAGE  "Vprune dig=input dig_out=output thresh=value[i]\n"

long ftell ();
double atof ();
double threshold;
int inches;

/*
#define DEBUG
*/
/*  command line args */
static	char  *dig_name = NULL ;
static	char  *out_name = NULL ;

static  int  load_args() ;

struct Command_keys vars[] = {
 { "dig_in", 1 },
 { "dig_out", 2 },
 { "threshold", 3 },
 { "input", 1 },
 { "output", 2 },
 { "thresh", 3 },
 { NULL,     0 }
};

double dig_unit_conversion ();
static	int   snapped = 0 ;

main (argc, argv)
    int argc;
    char **argv;
{
    int   ret ;
    char *mapset;


/*  check args and set flags  */
	
    threshold = 0.0;
    ret = G_parse_command (argc, argv, vars, load_args) ;
    if (ret > 0)	/* Help was requested */
         exit (1);

    if (ret < 0  ||  dig_name == NULL  ||   out_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

    if (threshold == 0.0)
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

/* Show advertising */
    G_gisinit(argv[0]) ;
    printf("\n\n   Prune:   threshold = %lf\n\n", threshold) ;

    if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
	G_fatal_error ("Could not find DIG file %s\n", dig_name);
    
    export (dig_name, mapset, out_name); 
    exit (0);
}

static
load_args (position, str)
    int position;
    char *str;
{
    switch(position)
    {
	case 1:
		dig_name = G_store(str) ;
		break ;
	case 2:
		out_name = G_store(str) ;
		break ;
	case 3:
		threshold = atof (str) ;
		if (str[strlen(str)-1] == 'i')
		    inches = 1;
		else 
		    inches = 0;
		break ;
	default:
		break;
    }	/*  switch  */

    return (0);
}

#ifdef DEBUG
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif


struct Map_info Map;
struct head Head;

export(dig_name, mapset, out_name)
    char *dig_name, *mapset, *out_name;
{
	FILE *Out;
	FILE *In;
	char buf[1024];

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	if (NULL == (In = G_fopen_old ("dig", dig_name, mapset)))
	{
	    fprintf (stderr, "Cannot open input file.\n");
	    exit (-1);
	}

	Out = G_fopen_new ("dig", out_name);

	dig_read_head_binary (In, &Head);
	dig_write_head_binary (Out, &Head);

	/* fix up threshold value to correspond to digit */
	if (inches)
	{
	    threshold = threshold * dig_unit_conversion() * Head.orig_scale;
	}
	/* in either case, take this resultant map value and div by 2 */
	threshold = threshold / 2.;

	doit (In, Out);

	fclose (Out);
	fclose (In);

	fprintf (stderr, "\n\nCopying Attribute file\n");

	if (NULL == (In = G_fopen_old ("dig_att", dig_name, mapset)))
	{
	    fprintf (stderr, "Cannot find attribute file.\n");
	    exit (1);
	}
	Out = G_fopen_new ("dig_att", out_name);

	while (NULL != fgets (buf, sizeof (buf), In))
	    fputs (buf, Out);
	    
	fclose (Out);
	fclose (In);

	fprintf (stderr, "Done.\n");


	return(0) ;
}

doit (in, out)
    FILE *in;
    FILE *out;
{
    struct line_pnts Points;
    register int line, type;
    int binary;
    long offset;
    int diff;
    int left;
    int old, new; 
    
    Points.alloc_points = 0;

/*DEBUG*/ fprintf (stderr, "Resultant threshold = %lf\n", threshold);
    left = diff = 0;
    line = 0;
    while (1)
    {
	line++;
	if (line % 10 == 0)
	    fprintf (stderr, "Pruning line %5d  pruned: %d  left; %d\r", line, diff, left);
	offset = ftell (in);
	if (0 > (type = dig__Read_line (&Points, in, offset)))
	{
	    if (type == -1)
	    {
		fprintf (stderr, "Out of memory on line %d\n", line);
		return (-1);
	    }
	    else 	/* EOF */
		return (0);
	}
	old = Points.n_points;
	Points.n_points = dig_prune (&Points, threshold);
	diff += old - Points.n_points;
	left += Points.n_points;
	dig__Write_line (out, type, &Points);
    }
}

double sample_thresh ;

struct head
{
	char organization[30] ;
	char date[20] ;
	char your_name[20] ;
	char map_name[41] ;
	char source_date[11] ;
	long  orig_scale ;
	char line_3[73] ;
	int plani_zone ;
	double W, E, S, N ;
	double digit_thresh ;
	double map_thresh ;
} head ;
#endif /*FOOLISH*/

