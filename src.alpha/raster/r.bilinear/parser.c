# include "gis.h"
# include "bilinear.h"

getargs (argc, argv)
    char *argv[];
{
  struct
  {
    struct Option *input, *output, *north, *south;
  } param;

    struct Flag *flag1 ;

    /* Define the different options */

    param.input = G_define_option() ;
    param.input->key        = "input";
    param.input->type       = TYPE_STRING;
    param.input->required   = YES;
    param.input->gisprompt  = "old,cell,raster" ;
    param.input->description= "Name of an input layer" ;

    param.output = G_define_option() ;
    param.output->key        = "output";
    param.output->type       = TYPE_STRING;
    param.output->required   = YES;
    param.output->gisprompt  = "new,cell,raster" ;
    param.output->description= "Name of an output layer";

    param.north = G_define_option() ;
    param.north->key        = "north";
    param.north->type       = TYPE_INTEGER;
    param.north->required   = NO;
    param.north->description= "Raster value at north pole";

    param.south = G_define_option() ;
    param.south->key        = "south";
    param.south->type       = TYPE_INTEGER;
    param.south->required   = NO;
    param.south->description= "Raster value at south pole";
 
    /* Define the different flags */
 
    flag1 = G_define_flag() ;
    flag1->key         = 'q' ;
    flag1->description = "Quiet" ;

    if (G_parser(argc, argv))
        exit (-1);

    verbose = (! flag1->answer);
	npole_set = param.north->answer ? 1 : 0;
	spole_set = param.south->answer ? 1 : 0;

    strcpy (name, param.input->answer);
    strcpy (result, param.output->answer);
    mapset = G_find_cell2 (name, "");

    if (mapset == NULL)
    {
        char buf[200];
        sprintf (buf, "cell file [%s] not found", name);
        G_fatal_error (buf);
    }

    if (G_legal_filename (result) < 0)
    {
        char buf[200];
        sprintf (buf, "[%s] illegal name", result);
        G_fatal_error (buf);
    }

    if (npole_set && sscanf(param.north->answer,"%d", &npole) != 1)
    {
        fprintf (stderr, "%s=%s - illegal north pole value\n",
                param.north->key, param.north->answer);
        G_usage();
        exit(1);
    }

    if (spole_set && sscanf(param.south->answer,"%d", &spole) != 1)
    {
        fprintf (stderr, "%s=%s - illegal south pole value\n",
                param.south->key, param.south->answer);
        G_usage();
        exit(1);
    }
	
	return 1;
}

