/* ****************************************************************************
 *
 * MODULE:       v.label 
 * AUTHOR(S):    Philip Verhagen (original s.label), Radim Blazek
 * PURPOSE:      Create paint labels    
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

void letter_position (double, struct line_pnts *, double *, double *, double *);

main (argc, argv)
int argc;
char **argv;

{
    int    i, j, ret, level, cnt, row, line, txtlength;
    int    type, ltype;
    int    cat, direction;
    double x, y, dx, dy, linlength, lablength, len, space, ldist, dist, ddist;
    double k, rotate, rot;
    char   *mapset;
    char   errmsg[1000], buf[2000];
    char   *txt, text[2000];
    struct line_pnts *Points;
    FILE   *labels, *texts;
    
    struct Map_info Map;
    struct GModule *module;
    struct Option *Vectfile, *Typopt;
    struct Option *Textfile;

    struct Option *Labelfile;
    struct Option *Xoffset;
    struct Option *Yoffset;
    struct Option *Reference;
    struct Option *Font;
    struct Option *Color;
    struct Option *Size;
    struct Option *Width;
    struct Option *Hcolor;
    struct Option *Hwidth;
    struct Option *Bcolor;
    struct Option *Border;
    struct Option *Opaque;
    
    struct Option *Space;

    G_gisinit(argv[0]);
    module = G_define_module();
    module->description = "Create paint labels for binary GRASS vector file.";

    Vectfile = G_define_option();
    Vectfile->key 		= "map";
    Vectfile->type		= TYPE_STRING;
    Vectfile->required		= YES;
    Vectfile->multiple		= NO;
    Vectfile->gisprompt		= "old,dig,Vector";
    Vectfile->description	= "vector file";

    Typopt = G_define_option();
    Typopt->key              = "type";
    Typopt->type             =  TYPE_STRING;
    Typopt->required         =  NO;
    Typopt->answer           =  "line";
    Typopt->options          =  "line";
    Typopt->description      =  "Select point or line";
    
    Textfile = G_define_option();
    Textfile->key = "text";
    Textfile->description = "Full path to file containing label texts in format: category text";
    Textfile->type = TYPE_STRING;
    Textfile->required = YES;

    Labelfile = G_define_option();
    Labelfile->key = "label";
    Labelfile->description = "Name of a paint-label file";
    Labelfile->type = TYPE_STRING;
    Labelfile->required = YES;

    Xoffset = G_define_option();
    Xoffset->key = "xoffset";
    Xoffset->description = "Offset label in x-direction";
    Xoffset->type = TYPE_INTEGER;
    Xoffset->answer = "";

    Yoffset = G_define_option();
    Yoffset->key = "yoffset";
    Yoffset->description = "Offset label in y-direction";
    Yoffset->type = TYPE_INTEGER;
    Yoffset->answer = "";

    Reference = G_define_option();
    Reference->key = "reference";
    Reference->description = "Reference position";
    Reference->type = TYPE_STRING;
    Reference->answer = "center";
    Reference->options = "center,left,right,upper,lower";

    Font = G_define_option();
    Font->key = "font";
    Font->description = "Font";
    Font->type = TYPE_STRING;
    Font->answer = "standard";

    Size = G_define_option();
    Size->key = "size";
    Size->description = "Label size (in map-units)";
    Size->type = TYPE_INTEGER;
    Size->answer = "100";
    Size->options = "1-1000";

    Color = G_define_option();
    Color->key = "color";
    Color->description = "Text color";
    Color->type = TYPE_STRING;
    Color->answer = "black";
    Color->options = "aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

    Width = G_define_option();
    Width->key = "width";
    Width->description = "Line width of text (only for p.map output)";
    Width->type = TYPE_INTEGER;
    Width->answer = "1";
    Width->options = "1-100";

    Hcolor = G_define_option();
    Hcolor->key = "hcolor";
    Hcolor->description = "Highlight color for text (only for p.map output)";
    Hcolor->type = TYPE_STRING;
    Hcolor->answer = "none";
    Hcolor->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

    Hwidth = G_define_option();
    Hwidth->key = "hwidth";
    Hwidth->description = "Line width of highlight color (only for p.map output)";
    Hwidth->type = TYPE_INTEGER;
    Hwidth->answer = "0";
    Hwidth->options = "0-100";

    Bcolor = G_define_option();
    Bcolor->key = "background";
    Bcolor->description = "Background color";
    Bcolor->type = TYPE_STRING;
    Bcolor->answer = "none";
    Bcolor->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

    Border = G_define_option();
    Border->key = "border";
    Border->description = "Border color";
    Border->type = TYPE_STRING;
    Border->answer = "none";
    Border->options = "none,aqua,black,blue,brown,cyan,gray,green,grey,indigo,magenta, orange,purple,red,violet,white,yellow";

    Opaque = G_define_option();
    Opaque->key = "opaque";
    Opaque->description = "Opaque to vector (only relevant if background color is selected)";
    Opaque->type = TYPE_STRING;
    Opaque->answer = "yes";
    Opaque->options = "yes,no";

    Space = G_define_option();
    Space->key = "space";
    Space->description = "Space between letters (in map-units)";
    Space->type = TYPE_DOUBLE;
    Space->answer = "100";
    Space->options = "1-100000";

    if (G_parser (argc, argv ) ) exit (-1 );

    ltype = 0;
    if (Typopt->answer[0] == 'p')
        ltype = DOT;
    else if (Typopt->answer[0] == 'l')
        ltype = LINE;

    /* why atof() fails ?*/
    space = atoi (Space->answer);

    /* open vector */	
    if (NULL == (mapset = G_find_file2 ("dig", Vectfile->answer, G_mapset())))
    {
	sprintf (errmsg, "Could not find file '%s'", Vectfile->answer);
	G_fatal_error (errmsg);
    }

    level = Vect_open_old (&Map, Vectfile->answer, mapset);
	
    if (level < 2)
    {
	fprintf (stderr, "v.support has not been run.\n");
        Vect_close (&Map);
	exit (1);
    }
    
    /* open texts */	
    texts = fopen (Textfile->answer, "r");
    if ( texts == NULL )
    {
	fprintf (stderr, "cannot open texts: %s\n", Textfile->answer);
        Vect_close (&Map);
	exit (1);
    }
    
    
    /* open labels */	
    labels = G_fopen_new ("paint/labels", Labelfile->answer);

    Points = Vect_new_line_struct ();

    /* write label */
    
    row = 0;
    cnt = 0;
    while ( NULL != ( fgets( buf, 2000,texts)) ) 
    {
        row++;
        cat = atoi (buf);
	txt = (char *) strchr (buf, ' ');
	
        if ( cat < 1 || txt == NULL )
        {
	    fprintf (stderr, "row %d text not correct: %s\n", row, buf);
	    G_warning (errmsg);
	    continue;
        }
	
	txt++;
	txt[strlen(txt)-1] = '\0';
	
        /* find the lines for that category */
	line = -1;
        for (line = 1 ; line <= V2_num_lines(&Map)  ; line++)
        {
	    if ( line < 0 ) continue;

            if ( 0 >= (type = V2_read_line(&Map, Points, line)))
            {
	        fprintf (stderr, "could not label line %d\n",i);
	        continue;
	    }
	
	    if ( type != ltype || cat != V2_line_att(&Map, line) )
	    	continue;
	
	    txtlength = strlen(txt);
	
            linlength = 0;
	    for (i=0; i < Points->n_points - 1; i++)
	        linlength += G_distance(Points->x[i], Points->y[i], Points->x[i+1], Points->y[i+1]);
	
            /* find best orientation (most letters by bottom to down side */
	    rotate = 0;
	    for (i=0; i < txtlength; i++)
            {
                /* distance of the letter from the beginning of line */
	        lablength = txtlength * space;
	        ldist = i * space + ( linlength - lablength ) / 2; 
	
	        if ( ldist < 0 ) ldist = 0;
	        if ( ldist > linlength ) ldist = linlength;

                letter_position ( ldist, Points, &x, &y, &rot);
	        if (rot > 90 || rot < -90 )
	            rotate += -1;
	        else
	            rotate += 1;
	    }
	    if ( rotate >= 0 ) { direction = 0; } else { direction = 1; }

            for (i=0; i < txtlength; i++)
            {
                /* distance of the letter from the beginning of line */
	        lablength = txtlength * space;
	    
	        ldist = i * space + ( linlength - lablength ) / 2;

	        if ( ldist < 0 ) ldist = 0;
	        if ( ldist > linlength ) ldist = linlength;

                letter_position ( ldist, Points, &x, &y, &rotate);
	        if ( direction == 0 ) {
                    sprintf (text, "%c", txt[i]);
	        } else {
                    sprintf (text, "%c", txt[txtlength - i - 1]);
                    rotate += 180;
	        }
	    
	        fprintf (labels, "east: %f\n", x);
	        fprintf (labels, "north: %f\n", y);
	        fprintf (labels, "xoffset: %s\n", Xoffset->answer);
	        fprintf (labels, "yoffset: %s\n", Yoffset->answer);
	        fprintf (labels, "ref: %s\n", Reference->answer);
	        fprintf (labels, "font: %s\n", Font->answer);
	        fprintf (labels, "color: %s\n", Color->answer);
	        fprintf (labels, "size: %s\n", Size->answer);
	        fprintf (labels, "width: %s\n", Width->answer);
	        fprintf (labels, "hcolor: %s\n", Hcolor->answer);
	        fprintf (labels, "hwidth: %s\n", Hwidth->answer);
	        fprintf (labels, "background: %s\n", Bcolor->answer);
	        fprintf (labels, "border: %s\n", Border->answer);
	        fprintf (labels, "opaque: %s\n", Opaque->answer);
	        fprintf (labels, "rotate: %f\n\n", rotate);
	        fprintf (labels, "text: %s\n\n", text);
            }
	    cnt++;
        }
    }

    Vect_destroy_line_struct(Points);
    
    Vect_close (&Map);
    fclose (texts);
    fclose (labels);

    fprintf (stderr, "Labeled %d lines.\n", cnt);

    exit (0);
}

void letter_position ( double ldist, struct line_pnts *Points, double *x, double *y, double *rot)
{
    int j;
    int dist = 0;
    int ddist = 0;
    double dx, dy, k, len;
    

    for (j=0; j < Points->n_points - 1; j++)
    {
        ddist = G_distance(Points->x[j], Points->y[j], Points->x[j+1], Points->y[j+1]);
        len = ldist - dist; /* rest */
        if ( (dist + ddist) >= ldist ){ /* letter is on current line part */
            k = len / ddist;
		    
	    dx = Points->x[j+1] - Points->x[j];
            dy = Points->y[j+1] - Points->y[j];

	    *x = Points->x[j] + k * dx;
            *y = Points->y[j] + k * dy;
		    	    
	    /* calculate angle */
	    *rot = atan2(dy, dx) * 180 / 3.14;
	    break;
	} else {
	    dist += ddist; 
	}
    }
    return;
}	 

