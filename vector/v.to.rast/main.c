#include <stdio.h> 
#include <stdlib.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "local.h"

int main (int argc, char *argv[])
{
    struct GModule *module;
    struct Option *input, *output, *rows, *col, *field_opt, *use_opt, *val_opt;
    int    field, nrows, use, value_type;
    double value;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description = "Converts a binary GRASS vector map layer "
		          "into a GRASS raster map layer.";

    input = G_define_option();
    input->key             = "input";
    input->type            = TYPE_STRING;
    input->required        = YES;
    input->multiple        = NO;
    input->gisprompt       = "old,vector,vector";
    input->description     = "vector input file";

    field_opt = G_define_standard_option(G_OPT_V_FIELD);
    
    output = G_define_option();
    output->key            = "output";
    output->type           = TYPE_STRING;
    output->required       = YES;
    output->multiple       = NO;
    output->gisprompt      = "new,cell,raster";
    output->description    = "raster output file";

    use_opt = G_define_option();
    use_opt->key            = "use";
    use_opt->type           = TYPE_STRING;
    use_opt->required       = NO;
    use_opt->multiple       = NO;
    use_opt->options        = "attr,cat,val,z";
    use_opt->answer         = "attr";
    use_opt->description    = "Source of raster values:\n"
			"\t\tattr - read values from attribute table\n"
			"\t\tcat  - use category values\n"
			"\t\tval  - use value specified by value option\n"
			"\t\tz    - use z coordinate (points or contours only)";

    col = G_define_option();
    col->key            = "column";
    col->type           = TYPE_STRING;
    col->required       = NO;
    col->multiple       = NO;
    col->description    = "Column name";

    val_opt = G_define_option();
    val_opt->key              = "value";
    val_opt->type             = TYPE_DOUBLE;
    val_opt->required         = NO;
    val_opt->multiple         = NO;
    val_opt->answer           = "1";
    val_opt->description      = "Raster value";

    rows = G_define_option();
    rows->key              = "rows";
    rows->type             = TYPE_INTEGER;
    rows->required         = NO;
    rows->multiple         = NO;
    rows->description      = "number of rows to hold in memory";
    rows->answer           = "4096";

    if (G_parser (argc, argv)) exit (1);

    field = atoi (field_opt->answer);
    nrows = atoi (rows->answer);

    if ( use_opt->answer[0] == 'a' ) {
	use = USE_ATTR;
	if ( !col->answer )
    	    G_fatal_error ("col parameter missing (or use value parameter)" );
    } else if ( use_opt->answer[0] == 'c' ) {
	use = USE_CAT;
    } else if ( use_opt->answer[0] == 'v' ) {
	use = USE_VAL;
    } else if ( use_opt->answer[0] == 'z' ) {
	use = USE_Z;
    }

    value = atof ( val_opt->answer );
    if ( strchr ( val_opt->answer, '.') )
	value_type = USE_DCELL;
    else
	value_type = USE_CELL;
	

    exit( vect_to_rast (input->answer, output->answer, field, col->answer, nrows, use, value, value_type) );
}

