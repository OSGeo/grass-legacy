/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
**
**  GRASS4.0: converted for new parser - 1/91 - dks
*/
#include    <stdio.h>
#include    "/itc/grass/4.13/include/gis.h"
#include    "/itc/grass/4.13/include/Vect.h"
#include    "/itc/grass/4.13/include/dig_head.h"
#include    "/itc/grass/4.13/include/projects.h"
#include	<string.h>
#include	<ctype.h>


#define MAIN

/* 
USAGE:  v.out.dlg input=vector file   output=dlg file  
*/

/*
#define DEBUG
*/

double dig_unit_conversion ();
static	int   snapped = 0 ;
int dlgtype = 0;  /* < 5 will build UNIV poly */
int dlgext = 0; /* naming extension full=0 quarter:ne=1,nw=2,sw=3,se=4 oversize=5 */
int dlgnad=0; /* nad27 = blank nad83 = 1 in line 4 col 69*/
float dlgsize_e = 0.125, dlgsize_n = 0.125; /* set to full quad format */
char *dlgprefix, dlgprefix2[2], buf[20];
struct pj_info info_in, info_out, info_in, info_out;
struct Key_Value *in_proj_keys, *in_unit_keys;
struct Key_Value *out_proj_keys, *out_unit_keys;


main(argc, argv)
    int argc;
    char **argv;
{
    char *dig_name, *dlg_name;
    char *mapset;
    char errmsg[200];
    struct Option *old, *new, *type, *name, *dates, *scale, *qformat, *format_ex, *nad;
    char dig_name1[20], dlg_name1[20];


/* Show advertising */
    G_gisinit(argv[0]) ;

    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    old->gisprompt		= "old,dig,vector";
    old->description		= "vector input file";
    
    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
    new->gisprompt		= "new,dlg,dlg";
    new->description		= "DLG-3 Optional format output file";

    nad = G_define_option();
    nad->key		= "nad";
    nad->type		= TYPE_INTEGER;
    nad->required		= NO;
    nad->multiple		= NO;
    nad->description	= "North American Datum (27, 83)";
    nad->options		= "27,83";

    type = G_define_option();
    type->key			= "type";
    type->type			= TYPE_STRING;
    type->required		= NO;
    type->multiple		= NO;
    type->description		= "Type of DLG data to format (ssurgo,specfeat,hydro,culture,nrcs)";
    type->options		= "ssurgo,specfeat,hydro,culture,nrcs";

    name = G_define_option();
    name->key			= "mapname";
    name->type			= TYPE_STRING;
    name->required		= NO;
    name->multiple		= NO;
    name->description		= "Name of the map";

    dates = G_define_option();
    dates->key			= "dates";
    dates->type			= TYPE_STRING;
    dates->required		= NO;
    dates->multiple		= NO;
    dates->description		= "Source and revision dates";

    scale = G_define_option();
    scale->key			= "scale";
    scale->type			= TYPE_INTEGER;
    scale->required		= NO;
    scale->multiple		= NO;
    scale->description		= "Map scale";

    qformat = G_define_option();
    qformat->key		= "qformat";
    qformat->type		= TYPE_STRING;
    qformat->required		= NO;
    qformat->multiple		= NO;
    qformat->description	= "Quadrangle format (full,quarter,oversize)";
    qformat->options		= "full,quarter,oversize";

    format_ex = G_define_option();
    format_ex->key		= "format_ex";
    format_ex->type		= TYPE_STRING;
    format_ex->required		= NO;
    format_ex->multiple		= NO;
    format_ex->description	= "Quadrangle format extension(f=0,qne=1,qnw=2,qsw=3,qse=4,ov=5)";
    format_ex->options		= "0,1,2,3,4,5";

    if (G_parser (argc, argv))
	exit (-1);

    dig_name = old->answer;
    dlg_name = new->answer;

    if (!*dig_name  || !*dlg_name )
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }
    if (type->answer != NULL){
/*printf("%d\n",strlen(new->answer));*/
	if(strlen(new->answer) > 12)
		G_fatal_error("Output file name too long. Must be < 13 char.");
    	if (strcmp(type->answer, "ssurgo") == 0){
		dlgtype = 5;
	}else if (strcmp(type->answer, "specfeat") == 0){
		dlgtype = 6;
	}else if (strcmp(type->answer, "hydro") == 0){
		dlgtype = 7;
	}else if (strcmp(type->answer, "culture") == 0){
		dlgtype = 8;
	}else if (strcmp(type->answer, "nrcs") == 0){
		dlgtype = 1;
	}
    }

/* added RLG, 1/12/1998 */
    if (dlgtype != 0 && dlgtype != 1){

    if (qformat->answer != NULL){
	if(strlen(new->answer) > 9)
		G_fatal_error("Format name too long. Must be < 12 char.");
    	if (strcmp(qformat->answer, "full") == 0){
		dlgsize_e = 0.125;
		dlgsize_n = 0.125;
	}else if (strcmp(qformat->answer, "quarter") == 0){
		dlgsize_e = 0.0625;
		dlgsize_n = 0.0625;
	}else if (strcmp(qformat->answer, "oversize") == 0){
		printf("\nEnter the quad format in MINUTES for the e-w: "); 
		scanf(" %s",buf);	
		dlgsize_e = atof(buf);
		printf("Enter the quad format in MINUTES for the n-s: ");
		scanf(" %s",buf);	
		dlgsize_n = atof(buf);
		dlgsize_e = dlgsize_e/60;
		dlgsize_n = dlgsize_n/60;
	}
    }

    if (format_ex->answer != NULL){
    	if (strcmp(format_ex->answer, "0") == 0){
		dlgext=0;
	}else if (strcmp(format_ex->answer, "1") == 0){
		dlgext=1;
	}else if (strcmp(format_ex->answer, "2") == 0){
		dlgext=2;
	}else if (strcmp(format_ex->answer, "3") == 0){
		dlgext=3;
	}else if (strcmp(format_ex->answer, "4") == 0){
		dlgext=4;
	}else if (strcmp(format_ex->answer, "5") == 0){
		dlgext=5;
 	}
    }
    } /* end of dlgtype 5,6,7,8 check */

    if (nad->answer != NULL){
    	if (strcmp(nad->answer, "27") == 0){
		dlgnad=0;
	}else if (strcmp(nad->answer, "83") == 0){
		dlgnad=1;
        }
    }

    printf("\n\n   Export.DLG:\n\n") ;

    if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
    {
	sprintf ("Could not find Vector file <%s>\n", dig_name);
	G_fatal_error (errmsg);
    }
        in_proj_keys = G_get_projinfo();
        if ( in_proj_keys == NULL ){
                G_fatal_error("Error reading PROJ_INFO file.");
                exit (0);
        }
        in_unit_keys = G_get_projunits();
        if ( in_unit_keys == NULL ){
                G_fatal_error("Error reading PROJ_UNITS file.");
                exit (0);
        }
        if (pj_get_kv( &info_in, in_proj_keys, in_unit_keys) < 0) {
                G_fatal_error("Error getting in proj key values.");
                exit (0);
        }
        out_proj_keys = G_create_key_value();
        out_unit_keys = G_create_key_value();
        G_set_key_value("name", "Latitude-Longitude", out_proj_keys);
        G_set_key_value("proj", "ll", out_proj_keys);
        /* keep ellps same as input */
        G_set_key_value("ellps", G_find_key_value("ellps", in_proj_keys), out_proj_keys);
        G_set_key_value("unit", "degree", out_unit_keys);
        G_set_key_value("units", "degrees", out_unit_keys);
        G_set_key_value("meters", "1.0", out_unit_keys);
 
        if (pj_get_kv( &info_out, out_proj_keys, out_unit_keys) < 0) {
                G_fatal_error("Error getting out proj key values.");
                exit (0);
        }

    switch (dlgtype){
	case 5:
	case 6:
	case 7:
	case 8:
		if (strcmp(G_find_key_value("proj", in_proj_keys), "utm") != 0)
			G_fatal_error("Not UTM location.");
		break;
	}
    
    export (dig_name, mapset, dlg_name, dlgtype, dlgnad, dlgext, 
		 name->answer, dates->answer, scale->answer, 
		 dlgsize_e, dlgsize_n); 
    exit (0);
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

export(dig_name, mapset, dlg_name, dlg_type, dlg_ext, dlg_nad, map_name, map_dates, map_scale)
    char *dig_name, *mapset, *dlg_name, *dlg_type, *dlg_ext, *dlg_nad;
    char *map_name, *map_dates, *map_scale;
{
	FILE *out;
	int att, line;
	char out_name[250],buf[250];
	register int num, i, j, k;
	char *X, *Y;
	int n_points;
	int level;
	char *cptr;

	if ( ! mapset)
	{
		G_fatal_error ("No mapset specified.\n");
	}

	/*
	dig_P_init (dig_name, mapset, &Map);
	*/
	level = Vect_open_old (&Map, dig_name, mapset);
	if (level <= 0)
	    G_fatal_error ("Can't open vector file");

	if (level < 2 || !Map.all_areas || !Map.all_isles)
	{
	    fprintf (stderr, "\n\nYou must first run v.support (option 1) on this data.\n");
	    exit (1);
	}
	G__make_mapset_element("dlg") ;
	G__file_name(out_name, "dlg", dlg_name, G_mapset()) ;
	dlgprefix = out_name;
	sprintf (dlgprefix2,".%d",dlgext);
	switch (dlgtype){
	case 5:
		G_strcpy(buf, dlgprefix);
		strcat(buf, dlgprefix2);
		strcat(buf, "af");
		out = fopen(buf, "w");
		break;
	case 6:
		G_strcpy(buf, dlgprefix);
		strcat(buf, dlgprefix2);
		strcat(buf, "sf");
		out = fopen(buf, "w");
		break;
	case 7:
		G_strcpy(buf, dlgprefix);
		strcat(buf, dlgprefix2);
		strcat(buf, "hf");
		out = fopen(buf, "w");
		break;
	case 8:
		G_strcpy(buf, dlgprefix);
		strcat(buf, dlgprefix2);
		strcat(buf, "cf");
		out = fopen(buf, "w");
		break;
	default:
		out = fopen (out_name, "w");
		break;
	}

	/*
	dig_read_head_binary (Map.digit, &D_head);
	*/

  	switch (dlgtype){
	   case 5:
	   case 6:
	   case 7:
	   case 8:
		break;
	   case 1:
	   default:
		build_area_one (&Map);
		shuffle_dots (&Map);
		break;
	}
	if (map_name != NULL){
		cptr = map_name;
		while( *cptr != 0 ){
			if(islower(*cptr))
				*cptr = toupper(*cptr);
			cptr++;
		}
		G_strncpy(Map.head.map_name, map_name,41);
	}
	if (map_dates != NULL)
		G_strncpy(Map.head.source_date, map_dates,11);
	if (map_scale != NULL){
		/* get rid of any 1:24000 stuff */
		cptr = map_scale;
		if ((cptr = strchr(map_scale, ':')) != NULL){
			map_scale = cptr + 1;
		}
		/* check for 24,000 */
		/* map_scale is in the right place after any : now */
		cptr = map_scale;
		for (j=0,k=0;*cptr != 0; j++, cptr++){
			if(isdigit(*cptr)){
				buf[k++] = *cptr;	
			}
		}
                buf[k] = 0;
		map_scale = buf;
		Map.head.orig_scale = atol(map_scale);
	}
	printf ("Writing Header information\n");
	write_dlg_head (&Map, &(Map.head), out);
	printf ("Writing Node information\n");
	write_dlg_nodes (&Map, out);
	printf ("Writing Area information\n");
	write_dlg_areas (&Map, out);
	printf ("Writing Line information\n");
	write_dlg_lines (&Map, out);

	fclose (out);
	/*
	dig_P_fini (&Map);
	*/
	Vect_close (&Map);
	printf ("Done.\n");

	return(0) ;
}
