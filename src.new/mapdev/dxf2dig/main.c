/* %W% %G% */
 
/*
**  Original written by Chuck Ehlshlaeger  6/89
**  Revised by Dave Gerdes  12/89
**  US Army Construction Engineering Research Lab
*/
#include    <stdio.h>
#include    <math.h>
#include    "gis.h"

#define MAIN
#include "dxf2vect.h"
#define  USAGE  "dxf2dig dxf=file [lines=n,n:x,!a..] [labels=n,n...] [prefix=out.prefix]"


/* gotta change this to be malloced */
#define MAX_ALLOC 100
char *line_list[MAX_ALLOC][2];
char *label_list[MAX_ALLOC][2];
int num_lines = 0 , num_labels = 0;

/*
#define DEBUG
*/
/*  command line args */
static	char  *out_name = NULL ;
char *rindex ();

int all_lines = 1;	/* dump ALL lines unless user override */
int all_atts = 1;	/* dump ALL atts  unless user override */

int from_table = 0;

static  int  load_args() ;

struct Command_keys vars[] = {
 { "dxf", 1 },
 { "lines", 2 },
 { "labels", 3 },
 { "prefix", 4 },
 { NULL,     0 }
};

main(argc, argv)
    int argc;
    char *argv[];
{
    int   ret ;
    FILE *dxf_fp;
    char *p;

	
    G_gisinit(argv[0]);
    debuginit();

/*  check args and set flags  */
    ret = G_parse_command (argc, argv, vars, load_args) ;
    if (ret > 0)	/* Help was requested */
         exit (1);


    if (ret < 0  ||  dxf_file == NULL )
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
		argv[0], USAGE);
        exit (-1);
    }

    if ((dxf_fp = fopen (dxf_file, "r")) == NULL)
    {
	    fprintf (stderr, "\ncannot open [%s] for dxf file\n", dxf_file);
	    exit (-2);
    }

    /* check the number of lines in the file so G-percent can be used while
    ** program is running
    */

    fseek(dxf_fp,0L,2);
    file_size = ftell(dxf_fp);
    rewind(dxf_fp);
    fprintf(stderr,"\nCONVERSION OF %s TO DIG FILE:  ",dxf_file);
    if (file_size < 500000)
	percent = 10;
    else
	percent = 5;
    G_percent(0,file_size,percent);/* initializing variables inside G_percent */

    /* make basename from name of dxf file.  This will be used as
    ** the prefix for all layers that are created to avoid layer name
    ** conflicts between dxf files
    */
    if (out_name != NULL)
	strcpy (basename, out_name);
    else
    {
	p = rindex (dxf_file, '/');
	if (p == NULL)
	    p = dxf_file;
	else
	    p++;
	strcpy (basename, p);
	if (NULL != (p = rindex (basename, '.')))
	    if (p != basename)
		*p = '\0';  /* knock off any suffix */
    }

    sprintf (dig_path, "%s/%s/%s", G_gisdbase(), G_location(), G_mapset());

    /* jcm
    sprintf (command, "mkdir %s/dig_ascii > /dev/null", dig_path);
    system (command);
    sprintf (command, "mkdir %s/dig_att > /dev/null", dig_path);
    system (command);
    */
    dxf_init_chars ();
    dxf_find_lines (dxf_fp);
    fclose (dxf_fp);
    if (BOUNDARIES == 4)
	dxf_add_boundaries ();/*extents of map were located in dxf file */
    else
	dxf_add_extents (); /*extents of map calculated as points were read in*/

    exit (0);
}

static
load_args (position, str)
    int position;
    char *str;
{
    switch (position)
    {
	case 1:
		dxf_file = G_store(str) ;
		break ;
	case 2:
		add_line_layer (str);
		from_table = 1;
		break ;
	case 3:
		add_att_layer (str);
		from_table = 1;
		break;
	case 4:
		out_name = G_store(str) ;
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

add_line_layer (str)
    char *str;
{
    add_layer (str, line_list, &num_lines);
}

add_att_layer (str)
    char *str;
{
    add_layer (str, label_list, &num_labels);
}

add_layer (str, list, num)
    char *str;
    char *list[][2];
    int *num;
{
    char buf[200], *buf_p, *p = NULL;

    strcpy (buf, str);
    G_squeeze (buf);
    p = index (buf, ':');
    if (*buf == '!') 	/* not to be written out */
    {
	p = NULL;
	buf_p = buf+1;
    }
    else
    {
	buf_p = buf;
	if (p != NULL)	/* have an alias for output */
	{
	    *p = 0;
	    p++;
	}
	else
	    p = buf_p;	/* output is same as original layer */
    }

    _add_layer (list, num, buf_p, p);
}

_add_layer (list, num, from, to)
    char *list[][2];
    int *num;
    char *from, *to;
{
    list[*num][0] = G_store (from);
    if (to == NULL)
	list[*num][1] = NULL;
    else
	list[*num][1] = G_store (to);
    (*num)++;
}

char *
remap (str, type)
    char *str;
    int type;
{
    /*
    char *list[][2];
    */
    int num, i;

    if (!from_table)
	return (str);

    /* do lookups based on label remapping */
    if (type == DXF_LABEL_LINE)
	type = DXF_LABEL;

    if (type == DXF_ASCII)
    {
	/* list = line_list; */
	num = num_lines;
    }
    else
    {
	/* list = label_list; */
	num = num_labels;
    }
    

    if (type == DXF_ASCII)
    {
	for (i = 0 ; i < num ; i++)
	    if (!strcmp (str, line_list[i][0]))
		return (line_list[i][1]);
	return (NULL);
    }
    else
    {
	for (i = 0 ; i < num ; i++)
	    if (!strcmp (str, label_list[i][0]))
		return (label_list[i][1]);
	return (NULL);
    }
}



char *
dxf_fgets (buf, size, fp)
    char *buf;
    int size;
    FILE *fp;
{
    char *p;
    static int current_size =0;

    p = fgets (buf, size, fp);
    if (p != NULL)
    {
	current_size+= strlen(p);
	G_percent(current_size,file_size,percent);/* reporting status of job */
	G_squeeze (buf);
    }
    return (p);
}
