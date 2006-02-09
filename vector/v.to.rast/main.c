#include <stdio.h> 
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/dbmi.h>
#include <grass/Vect.h>
#include "local.h"
#include <grass/glocale.h>

int main (int argc, char *argv[])
{
    struct GModule *module;
    struct Option *input, *output, *rows, *col, *field_opt, *use_opt, *val_opt;
    int    field, nrows, use, value_type;
    double value;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description = _("Converts a binary GRASS vector map layer "
		          "into a GRASS raster map layer.");

    input  = G_define_standard_option(G_OPT_V_INPUT);
    output = G_define_standard_option(G_OPT_R_OUTPUT);

    use_opt = G_define_option();
    use_opt->key            = "use";
    use_opt->type           = TYPE_STRING;
    use_opt->required       = NO;
    use_opt->multiple       = NO;
    use_opt->options        = "attr,cat,val,z,dir";
    use_opt->answer         = "attr";
    use_opt->description    = _("Source of raster values:\n"
			"\t\tattr - read values from attribute table\n"
			"\t\tcat  - use category values\n"
			"\t\tval  - use value specified by value option\n"
			"\t\tz    - use z coordinate (points or contours only)\n"
			"\t\tdir  - output as flow direction (lines only)");

    col = G_define_option();
    col->key            = "column";
    col->type           = TYPE_STRING;
    col->key_desc       = "name";
    col->required       = NO;
    col->multiple       = NO;
    col->description    = _("Column name (type must be numeric)");

    field_opt = G_define_standard_option(G_OPT_V_FIELD);

    val_opt = G_define_option();
    val_opt->key              = "value";
    val_opt->type             = TYPE_DOUBLE;
    val_opt->required         = NO;
    val_opt->multiple         = NO;
    val_opt->answer           = "1";
    val_opt->description      = _("Raster value");

    rows = G_define_option();
    rows->key              = "rows";
    rows->type             = TYPE_INTEGER;
    rows->required         = NO;
    rows->multiple         = NO;
    rows->answer           = "4096";
    rows->description      = _("number of rows to hold in memory");

    if (G_parser (argc, argv))
	exit(EXIT_FAILURE);

    field = atoi (field_opt->answer);
    nrows = atoi (rows->answer);

    if ( use_opt->answer[0] == 'a' ) {
	use = USE_ATTR;
	if ( !col->answer )
    	    G_fatal_error (_("Column parameter missing (or use value parameter)") );
    } else if ( use_opt->answer[0] == 'c' ) {
	if ( col->answer )
    	    G_fatal_error (_("Column parameter cannot be combined with use of category values option") );
	use = USE_CAT;
    } else if ( use_opt->answer[0] == 'v' ) {
	if ( col->answer )
    	    G_fatal_error (_("Column parameter cannot be combined with use of value option") );
	use = USE_VAL;
    } else if ( use_opt->answer[0] == 'z' ) {
	if ( col->answer )
    	    G_fatal_error (_("Column parameter cannot be combined with use of z coordinate") );
	use = USE_Z;
    } else if ( use_opt->answer[0] == 'd' ) {
        use = USE_D;
    }

    value = atof ( val_opt->answer );
    if ( strchr ( val_opt->answer, '.') )
	value_type = USE_DCELL;
    else
	value_type = USE_CELL;
	

    exit( vect_to_rast (input->answer, output->answer, field, col->answer, nrows, use, value, value_type) );
}

