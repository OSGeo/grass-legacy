#include "global.h"

parse_command_line (argc, argv)
char *argv[];
{
	char pl_desc[256];
	char pw_desc[256];
	int i;
	struct
	    {
		struct Option *vect;
		struct Option *units;
		struct Option *type;
		struct Option *pl;    /* page length */
		struct Option *pw;    /* page width */
	} parms;
	struct
	    {
		struct Flag *f;
		struct Flag *h;
		struct Flag *q;
	} flags;

	parms.vect = G_define_option();
	parms.vect->key    = "map";
	parms.vect->type   = TYPE_STRING ;
	parms.vect->required = YES ;
	parms.vect->multiple = NO ;
	parms.vect->gisprompt  = "old,dig,vector" ;
	parms.vect->description = "vector map to report on";

        parms.type = G_define_option();
        parms.type->key          = "type";
        parms.type->description  = "type of vector map. ";
        parms.type->type         = TYPE_STRING;
        parms.type->required     = YES;
        parms.type->multiple     = NO;
        parms.type->options      = "area,line,site";

	parms.units = G_define_option();
	parms.units->key   = "units";
	parms.units->type   = TYPE_STRING ;
	parms.units->required = NO ;
	parms.units->multiple = YES ;
	parms.units->description =
	    "mi(les),f(eet),me(ters),k(ilometers),a(cres),h(ectacres),c(ounts)";

	parms.pl = G_define_option();
	parms.pl->key = "pl";
	parms.pl->type = TYPE_INTEGER;
	parms.pl->required = NO ;
	sprintf (pl_desc, "page length (default: %d lines)", DEFAULT_PAGE_LENGTH);
	parms.pl->description = pl_desc;

	parms.pw = G_define_option();
	parms.pw->key = "pw";
	parms.pw->type = TYPE_INTEGER;
	parms.pw->required = NO ;
	sprintf (pw_desc, "page width (default: %d characters)", DEFAULT_PAGE_WIDTH);
	parms.pw->description = pw_desc;

	flags.h = G_define_flag();
	flags.h->key = 'h';
	flags.h->description = "suppress page headers";

	flags.f = G_define_flag();
	flags.f->key = 'f';
	flags.f->description = "use formfeeds between pages";

	flags.q = G_define_flag();
	flags.q->key = 'q';
	flags.q->description = "quiet";

	if (G_parser(argc,argv))
		exit(-1);

	use_formfeed = flags.f->answer;
	with_headers = !flags.h->answer;
	verbose      = !flags.q->answer;

	if (parms.vect->answer)
		{
		parse_layer (parms.vect->answer);
		layers[0].type = parse_type(parms.type->answer);
		}

	if (parms.units->answers)
		for (i = 0; parms.units->answers[i]; i++)
		   parse_units (parms.units->answers[i],parms.type->answer);

	if (parms.pl->answer)
	{
		if (sscanf (parms.pl->answer, "%ld", &page_length) != 1 || page_length < 0)
		{
			fprintf (stderr, "Illegal page length\n");
			G_usage();
			exit(1);
		}
	}

	if (parms.pw->answer)
	{
		if (sscanf (parms.pw->answer, "%ld", &page_width) != 1 || page_width < 1)
		{
			fprintf (stderr, "Illegal page width\n");
			G_usage();
			exit(1);
		}
	}
}

parse_units (s,t)
char *s, *t;
{
	int x;

	if (match (s, "miles",2))
		{
		x = LN_MILES;
		if (match (t, "area",1)) x = SQ_MILES;
		}
	else if (match (s, "feet",1))
		{
		x = LN_FEET;
		if (match (t, "area",1)) x = SQ_FEET;
		}
	else if (match (s, "meters",2))
		{
		x = LN_METERS;
		if (match (t, "area",1)) x = SQ_METERS;
		}
	else if (match (s, "kilometers",1))
		{
		x = LN_KILOMETERS;
		if (match (t, "area",1)) x = SQ_KILOMETERS;
		}
	else if (match (s, "acres",1))
		{
		x = ACRES;
		if (!match (t, "area",1))
		 {
		 fprintf (stderr, "\nERROR: %s: Acres allowed with area data only\n",
		    G_program_name());
		 exit(-1);
		 }
		}
	else if (match (s, "hectacres",1))
		{
		x = HECTARES;
		if (!match (t, "area",1))
		 {
		 fprintf (stderr, "\nERROR: %s: Hectares allowed with area data only\n",
		    G_program_name());
		 exit(-1);
		 }
		}
	else if (match (s, "counts",1))
		x = COUNTS;
	else
	{
		G_usage();
		exit(1);
	}
	if (nunits >= MAX_UNITS)
	{
		fprintf (stderr, "\nERROR: %s: only %d unit%s allowed\n",
		    G_program_name(), MAX_UNITS, MAX_UNITS==1?"":"s");
		exit(1);
	}
	unit[nunits].type  = x;
	nunits++;
}

parse_type (s)
char *s;
{
	int x;
	if (match (s, "area",2))
		x = 1;
	else if (match (s, "line",2))
		x = 2;
	else if (match (s, "site",2))
		x = 3;
	else
	{
		G_usage();
		exit(1);
	}

	return(x);
}

parse_layer(s)
char *s;
{
	char msg[100];
	char name[200];
	char *mapset;
	int n;

	strcpy (name, s);
	mapset = G_find_vector (name, "");

	if (mapset == NULL)
	{
		sprintf (msg, "%s: <%s> vector map not found\n", G_program_name(), s);
		G_fatal_error (msg);
		exit(1);
	}

	n = nlayers++ ;
	layers = (LAYER *)G_realloc(layers, nlayers * sizeof(LAYER));
	layers[n].name = G_store (name);
	layers[n].mapset = mapset;
	if(G_read_vector_cats (name, mapset, &layers[n].labels))
	      G_init_cats(0," ", &layers[n].labels);
}

match (s, key, min)
char *s, *key;
{
	int len;

	if (!s) return 0;
	len = strlen (s);
	if (len < min) return 0;
	return strncmp (s, key, len) == 0;
}
