/*      will convert vector files into autocad files.  This program is
        a small demo and not to be taken seriously.                     

**      written by Chuck Ehlschlaeger                                   
**      improved March 15, 1989:  take it a little more seriously, but
        not too much.                                                   

**      Revised for Grass4.0- converted for new command parser 1/91 -dks
**         BUT--still needs to be set up for binary input before
	   moved from alpha to main installation.

**/

#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "dxf.h"
#include "local_proto.h"

/* size of text compared to screen=1					*/
#define TEXT_SIZE	.003
#define CENTERED	4
    
int 
main (int argc, char *argv[])
{
    double textsize;
    struct Option *old, *new;
    char   *mapset;
    struct GModule *module;
    struct Map_info In;

    G_gisinit (argv[0]);
    
    /* Set description */
    module              = G_define_module();
    module->description = "Exports GRASS vector files to DXF file format.";

    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    old->gisprompt		= "old,vector,vector";
    old->description		= "vector input file";
    
    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
    new->description		= "dxf output file";

    if (G_parser (argc, argv))
       exit (-1);

    /* open input vector */
    if ((mapset = G_find_vector2 (old->answer, "")) == NULL) {
        G_fatal_error ( "Could not find input map <%s>\n", old->answer);
    }
    
    Vect_set_open_level (2);
    Vect_open_old (&In, old->answer, mapset);

    /* open output */
    dxf_open ( new->answer );


    textsize = do_limits(&In);   /*does header in fpdxf                 */
    make_layername ();
    dxf_entities();
    add_plines ( &In, textsize );          /* puts plines in fpdxf                */
    dxf_endsec();
    do_eof();                    /* puts final stuff in fpdxf, closes file */
    exit(0);
}

double do_limits ( struct Map_info *Map )
{
    double textsize;
    BOUND_BOX box;

    Vect_get_map_box ( Map, &box );

    dxf_header();
    dxf_limits(box.N,box.S,box.E,box.W);
    dxf_endsec();

    if((box.E - box.W) >= (box.N - box.S))
        textsize = (box.E - box.W) * TEXT_SIZE;
    else	
	textsize = (box.N - box.S) * TEXT_SIZE;

    return(textsize);
}


int make_layername ( void )
{
    dxf_tables();
    dxf_linetype_table(1);
    dxf_solidline();
    dxf_endtable();
    dxf_layer_table(7);
    dxf_layer0();

    dxf_layer("point",1,"CONTINUOUS",0);
    dxf_layer("line",2,"CONTINUOUS",0);
    dxf_layer("boundary",3,"CONTINUOUS",0);
    dxf_layer("centroid",4,"CONTINUOUS",0);
    dxf_layer("point-label",5,"CONTINUOUS",0);
    dxf_layer("centroid-label",6,"CONTINUOUS",0);

    dxf_endtable();
    dxf_endsec();

    return 0;
}

int 
add_plines (struct Map_info *Map, double textsize )
{
    int nlines, line ;
    struct line_pnts *Points;
    struct line_cats *Cats;
    char   *layer, *llayer;
    int    cat;
    char   cat_num[50];

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    nlines = Vect_get_num_lines ( Map );

    for ( line = 1; line <= nlines; line++ ) {
	int i, ltype;

	ltype = Vect_read_line ( Map, Points, Cats, line);
	Vect_cat_get ( Cats, 1, &cat );
	sprintf ( cat_num, "%d", cat );

	if ( ltype == GV_POINT ) {
	    layer = "point";
	    llayer = "point-label";
	} else if ( ltype == GV_LINE ) {
	    layer = "line";
	} else if ( ltype == GV_BOUNDARY ) {
	    layer = "boundary";
	} else if ( ltype == GV_CENTROID ) {
	    layer = "centroid";
	    llayer = "centroid-label";
	} else {
	    continue;
	}

	if ( ltype & GV_POINTS ) {
	    dxf_point ( layer, Points->x[0], Points->y[0], Points->z[0] );
		    
	    dxf_text(llayer, Points->x[0], Points->y[0], Points->z[0] ,textsize, CENTERED, cat_num);
	} else { /* lines */
	    dxf_polyline ( layer );
	
	    for ( i = 0; i < Points->n_points; i++ ) {
		dxf_vertex ( layer, Points->x[i], Points->y[i], Points->z[i] );
	    }

	    dxf_poly_end ( layer );
	}
    }
    return (0);
}

int 
do_eof (void)
{
    dxf_eof();

    return 0;
}
