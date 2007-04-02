/* ****************************************************************************
 *
 * MODULE:       v.label.sa
 * AUTHOR(S):    Wolf Bergenheim
 * PURPOSE:      Create paint labels, but use a SA algorithm to avoit
 *               overlaping labels.
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/
#include <time.h>
#include "labels.h"
#define DEFAULT_CHARSET "UTF-8"

/**
 * This function writes the label information to the label file.
 * @param labelf An opened label file to append the label to.
 * @param l The label
 */
void print_label ( FILE *labelf, label_t *l);

/**
 * This function defines the parameters and calls the command-line parser */
int parse_args(int argc, char *argv[], struct params *p);

int main (int argc, char *argv[])
{
	struct params p;
	label_t *labels;
	int n_labels, i;
    struct GModule *module;

	srand((unsigned int)time(NULL));
	G_gisinit(argv[0]);
    module = G_define_module();
    module->keywords = _("vector, paint labels");
    module->description =
	_("Create optimally placed labels for vector map(s)");

printf("parsing options and flags\n");
	/* parse options and flags */
	if(parse_args(argc, argv, &p)) exit(EXIT_FAILURE);
printf("initialize labels\n");
	/* initialize labels (get text from database, and get features)*/
	labels = labels_init(&p, &n_labels);
	/* start algorithm */
	/*   1. candidate position generation */
	label_candidates(labels, n_labels);
	for(i=0; i < n_labels;i++) {
		int j;
		for(j=0; j < labels[i].n_candidates; j++) {
			printf("Label(%d), candidate(%d): %s cat=%d type=%d score=%lf (%.8lg,%.8lg)\n",
				   i, j, labels[i].text, labels[i].cat, labels[i].type,
				   labels[i].candidates[j].score,
				   labels[i].candidates[j].point.x,
				   labels[i].candidates[j].point.y);
		}
	}
	
	/*   2. position evaluation */
/*	label_candidate_overlap(labels, n_labels);*/
	/*   3. position selection */
	/* write lables to file */
	return EXIT_SUCCESS;
}

int parse_args(int argc, char *argv[], struct params *p)
{
	p->map = G_define_standard_option(G_OPT_V_MAP);

	p->type = G_define_standard_option(G_OPT_V_TYPE);
    p->type->options = "point,line,area";
    p->type->answer  = "point,line,area";

	p->layer = G_define_standard_option(G_OPT_V_FIELD);
	
    p->column = G_define_option() ;
    p->column->key         = "column" ;
    p->column->type        = TYPE_STRING ;
    p->column->required    = YES;
    p->column->description = _("Name of attribute column to be used for labels");
	
    p->labels = G_define_option();
    p->labels->key = "labels";
    p->labels->description = _("Name for new paint-label file");
    p->labels->type = TYPE_STRING;
    p->labels->required = YES;
    p->labels->key_desc = "name";

	p->font = G_define_option();
	p->font->key         = "path";
	p->font->type        = TYPE_STRING;
	p->font->required    = YES;
	p->font->description = _("Path to TrueType font (including file name)");
    p->font->guisection = _("Font");
	p->font->gisprompt   = "old_file,file,font";

    p->size = G_define_option();
    p->size->key = "size";
    p->size->description = _("Label size (in map-units)");
    p->size->type = TYPE_DOUBLE;
    p->size->answer = "100";
    p->size->guisection = _("Font");

	p->charset = G_define_option();
	p->charset->key         = "charset";
	p->charset->type        = TYPE_STRING;
	p->charset->required    = NO;
	p->charset->answer      = DEFAULT_CHARSET;
	p->charset->description = "Character encoding (default: "DEFAULT_CHARSET")";

    return G_parser (argc, argv);
}

#if 0
    int    i, cnt, nrows, txtlength, field, more;
    int    type, ltype;
    int    cat, direction;
    double x, y, linlength, lablength, size, space, ldist;
    double rotate, rot;
    char   *mapset;
    char   *txt, buf[2000];
    struct line_pnts *Points;
    struct line_cats *Cats;
    FILE   *labels;
    
    struct Map_info Map;
    struct GModule *module;
    struct Option *Vectfile, *Typopt, *Fieldopt, *Colopt;
    struct Option *Labelfile, *Space, *FontSize, *Rotation;
    struct Flag   *Along_flag, *Curl_flag;

    struct field_info *fi;
    dbDriver *driver;
    dbString stmt, valstr;
    dbCursor cursor;
    dbTable  *table;
    dbColumn *column;


    Vectfile = 

    Typopt =     
    Fieldopt = 

    Colopt = G_define_option() ;
    Colopt->key         = "column" ;
    Colopt->type        = TYPE_STRING ;
    Colopt->required    = YES;
    Colopt->description = _("Name of attribute column to be used for labels");

    Along_flag = G_define_flag();
    Along_flag->key            = 'a';
    Along_flag->description    = _("Rotate labels to align with lines");
    Along_flag->guisection = _("Effects");

    Curl_flag = G_define_flag();
    Curl_flag->key             = 'c';
    Curl_flag->description     = _("Curl labels along lines");
    Curl_flag->guisection = _("Effects");

    Labelfile = G_define_option();
    Labelfile->key = "labels";
    Labelfile->description = _("Name for new paint-label file");
    Labelfile->type = TYPE_STRING;
    Labelfile->required = YES;
    Labelfile->key_desc = "name";

    Xoffset = G_define_option();
    Xoffset->key = "xoffset";
    Xoffset->description = _("Offset label in x-direction");
    Xoffset->type = TYPE_DOUBLE;
    Xoffset->answer = "0";
    Xoffset->guisection = _("Placement");

    Yoffset = G_define_option();
    Yoffset->key = "yoffset";
    Yoffset->description = _("Offset label in y-direction");
    Yoffset->type = TYPE_DOUBLE;
    Yoffset->answer = "0";
    Yoffset->guisection = _("Placement");

    Reference = G_define_option();
    Reference->key = "reference";
    Reference->description = _("Reference position");
    Reference->type = TYPE_STRING;
    Reference->multiple = YES;
    Reference->answer = "center";
    Reference->options = "center,left,right,upper,lower";
    Reference->guisection = _("Placement");

    Font = G_define_option();
    Font->key = "font";
    Font->description = _("Font name");
    Font->type = TYPE_STRING;
    Font->answer = "standard";
    Font->guisection = _("Font");

    Size = G_define_option();
    Size->key = "size";
    Size->description = _("Label size (in map-units)");
    Size->type = TYPE_DOUBLE;
    Size->answer = "100";
    Size->guisection = _("Font");

    Space = G_define_option();
    Space->key = "space";
    Space->description = _("Space between letters for curled labels (in map-units)");
    Space->type = TYPE_DOUBLE;
    Space->required = NO;
    Space->guisection = _("Font");

    FontSize = G_define_option();
    FontSize->key = "fontsize";
    FontSize->description = _("Label size (in points)");
    FontSize->type = TYPE_INTEGER;
    FontSize->required = NO;
    FontSize->options = "1-1000";
    FontSize->guisection = _("Font");

    Color = G_define_option();
    Color->key = "color";
    Color->description = _("Text color");
    Color->type = TYPE_STRING;
    Color->answer = "black";
    Color->options = "aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";
    Color->guisection = _("Colors");

    Rotation = G_define_option();
    Rotation->key = "rotation";
    Rotation->description = _("Rotation angle (degrees counter-clockwise from East)");
    Rotation->type = TYPE_DOUBLE;
    Rotation->required = NO;
    Rotation->options = "0-360";
    Rotation->answer = "0";
    Rotation->key_desc = "angle";
    Rotation->guisection = _("Placement");


    Opaque = G_define_option();
    Opaque->key = "opaque";
    Opaque->description =
	_("Opaque to vector (only relevant if background color is selected)");
    Opaque->type = TYPE_STRING;
    Opaque->answer = "yes";
    Opaque->options = "yes,no";
    Opaque->key_desc = "yes|no";
    Opaque->guisection = _("Colors");

    if (G_parser (argc, argv))
	exit (EXIT_FAILURE);

    if(Curl_flag->answer) Along_flag->answer = 1;


    size = atof (Size->answer);
    space = size;  /* default: set spacing according to letter size (map units) */
    rotate = atof (Rotation->answer);

    if( 0 != strcmp("0", Rotation->answer) )
	G_warning("Currently the rotation option only works correctly for left justified text.");

    if(FontSize->answer) {
	fontsize = atoi(FontSize->answer);

	/* figure out space param dynamically from current dispay */
	/* don't bother if Space was explicitly given (bypasses xmon req) */
	if(Along_flag->answer && ! Space->answer) {
	    if (R_open_driver() != 0)  /* connect to the driver */
		G_fatal_error(_("No graphics device selected"));

	    /* Read in the map region associated with graphics window */
	    D_setup(0);
	    space = fontsize / D_get_u_to_d_xconv();  /* in earth units */

	    R_close_driver();
	}
    }
    else
	fontsize = 0;

    /* or if user explicitly gave a number for letter spacing, use that */
    if(Space->answer)
	space = atof (Space->answer);

    if(Along_flag->answer && !fontsize && ( size/space >= 2  ||  size/space <= 0.5 ))
	G_warning(_("size and space options vary significantly which may lead to crummy output"));


    /* parse reference answers */
    i=0;
    strcpy(ref_pt,"");
    while(Reference->answers[i]) {
	if(i>1) G_fatal_error(_("Too many parameters for <reference>"));
	if(i>0) strcat(ref_pt, " ");
	strncat(ref_pt, Reference->answers[i], 7);
	i++;
    }

    /* write label */
    cnt = 0;

    while (1) {
	
	/* Line length */
	linlength = Vect_line_length ( Points );
	
	if ( ltype & GV_POINTS ) {
	    print_label (labels, Points->x[0], Points->y[0], rotate, txt);
	} else if ( !Along_flag->answer ) { /* Line, but not along */
	    /* get centre */
            Vect_point_on_line ( Points, linlength/2, &x, &y, NULL, NULL, NULL);
	    print_label (labels, x, y, rotate, txt);
	} else { /* Along line */

	    /* find best orientation (most letters by bottom to down side */
	    rotate = 0;
	    for (i=0; i < txtlength; i++)
	    {
		/* distance of the letter from the beginning of line */
		lablength = txtlength * space;
		ldist = i * space + ( linlength - lablength ) / 2; 

		if ( ldist < 0 ) ldist = 0;
		if ( ldist > linlength ) ldist = linlength;

		Vect_point_on_line ( Points, ldist, &x, &y, NULL, &rot, NULL);
		rot = rot * 180 / PI;
		if (rot > 90 || rot < -90 ) rotate += -1; else rotate += 1;
	    }
	    if ( rotate >= 0 ) { direction = 0; } else { direction = 1; }

	    if(Curl_flag->answer) {
		for (i=0; i < txtlength; i++) {
		    /* distance of the letter from the beginning of line */
		    lablength = txtlength * space;
	    
		    ldist = i * space + ( linlength - lablength ) / 2;

		    if ( ldist < 0 ) ldist = 0;
		    if ( ldist > linlength ) ldist = linlength;

		    Vect_point_on_line ( Points, ldist, &x, &y, NULL, &rotate, NULL);
		    rotate = rotate * 180 / PI;
		
		    if ( direction == 0 ) {
			sprintf (buf, "%c", txt[i]);
		    } else {
			sprintf (buf, "%c", txt[txtlength - i - 1]);
			rotate += 180;
		    }
		    print_label (labels, x, y, rotate, buf);
		}
	    }
	    else { /* same as above but take center value for placement & rotation */
		i = (int)(txtlength/2.0 + 0.5);
		lablength = txtlength * space;
		ldist = i * space + ( linlength - lablength ) / 2;
		if ( ldist < 0 ) ldist = 0;
		if ( ldist > linlength ) ldist = linlength;
		Vect_point_on_line ( Points, ldist, &x, &y, NULL, &rotate, NULL);
		rotate = rotate * 180 / PI;
		if ( direction != 0 ) rotate += 180;
		print_label (labels, x, y, rotate, txt);
	    }
	}
	cnt++;
    }

    Vect_destroy_line_struct(Points);
    
    Vect_close (&Map);
    db_close_database_shutdown_driver ( driver );
    fclose (labels);

    G_message( _("Labeled %d lines."), cnt);

    exit (EXIT_SUCCESS);
}

void print_label ( FILE *labels, double x, double y, double rotate, char *label) {
    fprintf (labels, "east: %f\n", x);
    fprintf (labels, "north: %f\n", y);
    fprintf (labels, "xoffset: %s\n", Xoffset->answer);
    fprintf (labels, "yoffset: %s\n", Yoffset->answer);
    fprintf (labels, "ref: %s\n", ref_pt);
    fprintf (labels, "font: %s\n", Font->answer);
    fprintf (labels, "color: %s\n", Color->answer);

    if(fontsize) fprintf (labels, "fontsize: %d\n", fontsize);
    else fprintf (labels, "size: %s\n", Size->answer);

    fprintf (labels, "width: %s\n", Width->answer);
    fprintf (labels, "hcolor: %s\n", Hcolor->answer);
    fprintf (labels, "hwidth: %s\n", Hwidth->answer);
    fprintf (labels, "background: %s\n", Bcolor->answer);
    fprintf (labels, "border: %s\n", Border->answer);
    fprintf (labels, "opaque: %s\n", Opaque->answer);
    if ( rotate != 0 ) 
        fprintf (labels, "rotate: %f\n", rotate);

    fprintf (labels, "text: %s\n\n", label);
}

/* Enable these later?
    Width = G_define_option();
    Width->key = "width";
    Width->description = _("Border width (only for ps.map output)");
    Width->type = TYPE_DOUBLE;
    Width->answer = "1";
    Width->guisection = _("Effects");

    Hcolor = G_define_option();
    Hcolor->key = "hcolor";
    Hcolor->description = _("Highlight color for text");
    Hcolor->type = TYPE_STRING;
    Hcolor->answer = "none";
    Hcolor->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";
    Hcolor->guisection = _("Colors");

    Hwidth = G_define_option();
    Hwidth->key = "hwidth";
    Hwidth->description = _("Width of highlight coloring");
    Hwidth->type = TYPE_DOUBLE;
    Hwidth->answer = "0";
    Hwidth->guisection = _("Effects");

    Bcolor = G_define_option();
    Bcolor->key = "background";
    Bcolor->description = _("Background color");
    Bcolor->type = TYPE_STRING;
    Bcolor->answer = "none";
    Bcolor->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";
    Bcolor->guisection = _("Colors");

    Border = G_define_option();
    Border->key = "border";
    Border->description = _("Border color");
    Border->type = TYPE_STRING;
    Border->answer = "none";
    Border->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";
    Border->guisection = _("Colors");
*/
#endif
