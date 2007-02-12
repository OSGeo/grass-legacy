/****************************************************************************
 *
 * MODULE:       v.to.rast
 * AUTHOR(S):    Original code: Michael Shapiro, U.S. Army Construction Engineering Research Laboratory
 *               Stream directions: Jaro Hofierka and Helena Mitasova
 *               Radim Blazek <radim.blazek gmail.com> (GRASS 6 update)
 *               Brad Douglas <rez touchofmadness.com>, Glynn Clements <glynn gclements.plus.com>,
 *               Hamish Bowman <hamish_nospam yahoo.com>, Markus Neteler <neteler itc.it>
 * PURPOSE:      
 * COPYRIGHT:    (C) 2003-2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/
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
    struct Option *input, *output, *rows, *col, *field_opt, *use_opt, *val_opt, *rgbcol_opt, *label_opt;
    struct Flag *table_acolors_flag, *table_labels_flag;
    int    field, nrows, use, value_type;
    double value;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->keywords = _("vector");
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
    col->description    = _("Column name (data type must be numeric)");

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

    rgbcol_opt = G_define_option();
    rgbcol_opt->key        = "rgb_column";
    rgbcol_opt->type       = TYPE_STRING ;
    rgbcol_opt->required   = NO ;
    rgbcol_opt->multiple   = NO ;
    rgbcol_opt->guisection = _("Colors");
    rgbcol_opt->description=
        _("Name of color definition column (for use with -a flag)");
    rgbcol_opt->answer     = "GRASSRGB" ;

    table_acolors_flag = G_define_flag ();
    table_acolors_flag->key		= 'a';
    table_acolors_flag->guisection  = _("Colors");
    table_acolors_flag->description	=
        _("Get colors from map table column (of form RRR:GGG:BBB)");

    label_opt = G_define_option();
    label_opt->key        = "label_column";
    label_opt->type       = TYPE_STRING ;
    label_opt->required   = NO ;
    label_opt->multiple   = NO ;
    label_opt->guisection = _("Labels");
    label_opt->description=
        _("Name of label definition column (for use with -l flag)");
    label_opt->answer     = "label" ;

    table_labels_flag = G_define_flag ();
    table_labels_flag->key		= 'l';
    table_labels_flag->guisection  = _("Labels");
    table_labels_flag->description	=
        _("Get labels from map table column");



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
	

    exit( vect_to_rast (input->answer, output->answer, field, col->answer, nrows, use, value, value_type, rgbcol_opt->answer, table_acolors_flag->answer, label_opt->answer, table_labels_flag->answer) );
}




