/****************************************************************
 * 
 *  MODULE:       v.net.alloc
 *  
 *  AUTHOR(S):    Radim Blazek
 *                
 *  PURPOSE:      Allocate subnets for nearest centres.
 *                
 *  COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * 
 *                This program is free software under the 
 *                GNU General Public License (>=v2). 
 *                Read the file COPYING that comes with GRASS
 *                for details.
 * 
 **************************************************************/
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"

typedef struct {
    int cat;   /* category number */
    int node;  /* node number */
} CENTER;

typedef struct {
    int    centre; /* neares centre, initialy -1 */
    double cost;   /* costs from this centre, initialy not undefined */
} NODE;

int main(int argc, char **argv)
{
    int    i, j, ret, centre, line, centre1, centre2;
    int    nlines, nnodes, type, ltype, afield, nfield, geo, cat;
    int    node, node1, node2;
    double cost;
    struct Option *map, *output;
    struct Option *afield_opt, *nfield_opt, *afcol, *abcol, *ncol, *type_opt, *term_opt;
    struct Flag   *geo_f;
    struct GModule *module;
    char   *mapset;
    struct Map_info Map, Out;
    struct cat_list *catlist;
    CENTER *Centers = NULL;
    int    acentres = 0, ncentres = 0;
    NODE   *Nodes;
    struct line_cats *Cats; 
    struct line_pnts *Points;

    G_gisinit (argv[0]) ;

    module = G_define_module();
    module->description = "Allocate subnets for nearest centres (direction from centre).";

    map = G_define_standard_option(G_OPT_V_INPUT);
    output = G_define_standard_option(G_OPT_V_OUTPUT); 

    type_opt =  G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options    = "line,boundary";
    type_opt->answer     = "line,boundary";
    type_opt->description = "Arc type";

    afield_opt = G_define_standard_option(G_OPT_V_FIELD);
    afield_opt->key = "afield";
    afield_opt->answer = "1";
    afield_opt->description = "Arc field";
    
    nfield_opt = G_define_standard_option(G_OPT_V_FIELD);
    nfield_opt->key = "nfield";
    nfield_opt->answer = "2";
    nfield_opt->description = "Node field";
    
    afcol = G_define_option() ;
    afcol->key         = "afcol" ;
    afcol->type        = TYPE_STRING ;
    afcol->required    = NO ; 
    afcol->description = "Arc forward/both direction(s) cost column" ;
    
    abcol = G_define_option() ;
    abcol->key         = "abcol" ;
    abcol->type        = TYPE_STRING ;
    abcol->required    = NO ; 
    abcol->description = "Arc backward direction cost column" ;
    
    ncol = G_define_option() ;
    ncol->key         = "ncol" ;
    ncol->type        = TYPE_STRING ;
    ncol->required    = NO ;
    ncol->description = "Node cost column" ;
    
    term_opt = G_define_standard_option(G_OPT_V_CATS);
    term_opt->key         = "ccats";
    term_opt->required    = YES;
    term_opt->description = "Categories of centres (points on nodes) to which net will be allocated, "
                            "field for this categories is given by nfield option.";
    
    geo_f = G_define_flag ();
    geo_f->key             = 'g';
    geo_f->description     = "Use geodesic calculation for longitude-latitude locations";
    
    if(G_parser(argc,argv)) exit (-1);

    Cats = Vect_new_cats_struct ();
    Points = Vect_new_line_struct ();

    type = Vect_option_to_types ( type_opt ); 
    afield = atoi (afield_opt->answer);
    nfield = atoi (nfield_opt->answer);

    catlist = Vect_new_cat_list ();
    Vect_str_to_cat_list ( term_opt->answer, catlist); 
    
    if ( geo_f->answer ) geo = 1; else geo = 0;
    
    mapset = G_find_vector2 (map->answer, NULL); 
      
    if ( mapset == NULL) G_fatal_error ("Could not find input %s\n", map->answer);
    Vect_set_open_level(2);
    Vect_open_old (&Map, map->answer, mapset); 
    
    nnodes = Vect_get_num_nodes ( &Map );

    /* Create list of centres based on list of categories */
    for (node = 1; node <= nnodes; node++) {
        nlines = Vect_get_node_n_lines ( &Map, node );    
        for (j = 0; j < nlines; j++) {
	    line = abs ( Vect_get_node_line ( &Map, node, j ) );
            ltype = Vect_read_line ( &Map, NULL, Cats, line);
	    if ( !(ltype & GV_POINT) ) continue; 
	    if ( !(Vect_cat_get(Cats, nfield, &cat)) ) continue; 
	    if ( Vect_cat_in_cat_list ( cat, catlist) ) {
		if ( acentres == ncentres ) {
		    acentres += 1;
		    Centers = (CENTER*) G_realloc ( Centers, acentres * sizeof(CENTER) );
		}
		Centers[ncentres].cat = cat;
		Centers[ncentres].node = node;
	        G_debug ( 2, "centre = %d node = %d cat = %d", ncentres, node, cat);
		ncentres++;
	    }  
        }
    } 
    fprintf ( stdout, "Number of centres: %d\n", ncentres );
    
    /* alloc and reset space for all nodes */
    Nodes = (NODE *) G_calloc ( ( nnodes + 1 ), sizeof(NODE) );
    for ( i = 1; i <= nnodes; i++ ){
	Nodes[i].centre = -1;
    }
    
    /* Build graph */
    Vect_net_build_graph ( &Map, type , afield, nfield, afcol->answer, abcol->answer, ncol->answer, geo, 0 );

    /* Fill Nodes by neares centre and costs from that centre */
    for ( centre = 0; centre < ncentres;  centre++ ) {
	node1 = Centers[centre].node;
	G_debug ( 2, "centre = %d node = %d cat = %d", centre, node1, Centers[centre].cat);
	for ( node2 = 1; node2 <= nnodes; node2++ ) {
	    G_debug ( 5, "  node1 = %d node2 = %d", node1, node2);
	    ret = Vect_net_shortest_path ( &Map, node1, node2, NULL, &cost);
	    if ( ret == -1 ) { continue; } /* node unreachable */
	    G_debug ( 5, "Arc nodes: %d %d cost: %f (x old cent: %d old cost %f", node1, node2, cost, 
		                                Nodes[node2].centre, Nodes[node2].cost);
	    if ( Nodes[node2].centre == -1 || cost < Nodes[node2].cost ) { 
		Nodes[node2].cost = cost;
		Nodes[node2].centre = centre;
	    }
	}
    }
    
    /* Write arcs to new map */
    Vect_open_new ( &Out, output->answer, Vect_is_3d (&Map) );

    nlines = Vect_get_num_lines ( &Map );
    for (line = 1; line <= nlines; line++) {
        ltype = Vect_read_line ( &Map, Points, NULL, line);
	if ( ! ( ltype & type ) ) { continue; }
	Vect_get_line_nodes ( &Map, line, &node1, &node2 );
	centre1 = Nodes[node1].centre;
	centre2 = Nodes[node2].centre;
	G_debug ( 5, "Arc centres: %d %d", centre1, centre2);
	if ( centre1 == centre2 ) { /* whole arc in one area */
	    Vect_reset_cats ( Cats );
	    if ( centre1 > -1 ) {
		cat = Centers[centre1].cat;
		Vect_cat_set ( Cats, 1, cat );
	    }
	    Vect_write_line ( &Out, ltype, Points, Cats );
	}
    }

    Vect_build (&Out, stdout);

    /* Free, ... */
    G_free ( Nodes );
    G_free ( Centers );
    Vect_close(&Map);
    Vect_close(&Out);

    exit(0);
}
