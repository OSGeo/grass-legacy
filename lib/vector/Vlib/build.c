/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.1 Radim Blazek and David D. Gray.
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "gis.h"
#include "Vect.h"

static int format () { G_fatal_error ("Requested format is not compiled in this version"); return 0; }

static int (*Build_array[]) () =
{
      Vect_build_nat
    , Vect_build_shp
#ifdef HAVE_POSTGRES
    , Vect_build_post
#else
    , format
#endif
#ifdef HAVE_OGR
    , Vect_build_ogr
#else
    , format
#endif
};

FILE *Msgout = NULL;

int prnmsg ( char *msg, ...) {
    char buffer[1000]; 
    va_list ap; 
    
    if ( Msgout != NULL ) {
        va_start(ap,msg);
	vsprintf(buffer,msg,ap);
	va_end(ap);
	fprintf (Msgout, "%s", buffer);
	fflush (Msgout);
    }
	
    return 1;	
}

/*!
 \fn int Vect_build ( struct Map_info *Map, FILE *msgout ) 
 \brief build topology for vector map
 \return 1 on success, 0 on error
 \param Map_info structure, file for message output (stdout/stderr for example) or NULL
*/
int
Vect_build ( struct Map_info *Map, FILE *msgout ) 
{
    struct Plus_head *plus ;
    int    ret;
    
    G_debug (1, "Vect_build()"); 

    /* If topology is already build (map on level2), set level to 1 so that lines will
    *  be read by V1_read_ (all lines) */
    Map->level = 1; /* may be not needed, because  V1_read is used directly by Vect_build_ */
    
    plus = &(Map->plus);
    prnmsg ("Building topology ...\n") ;
    dig_init_plus ( plus );
    plus->with_z = Map->head.with_z;
    plus->spidx_with_z = Map->head.with_z;
    
    ret = ( (*Build_array[Map->format]) (Map, msgout) );

    if ( ret == 0 ) { return 0; } 
    
    Map->level = LEVEL_2;
    plus->mode = GV_MODE_WRITE;
    
    prnmsg ("Topology was built.\n") ;
   
    prnmsg ("Number of nodes     :   %d\n", plus->n_nodes) ;
    prnmsg ("Number of primitives:   %d\n", plus->n_lines) ;
    prnmsg ("Number of points    :   %d\n", plus->n_plines) ;
    prnmsg ("Number of lines     :   %d\n", plus->n_llines) ;
    prnmsg ("Number of boundaries:   %d\n", plus->n_blines) ;
    prnmsg ("Number of centroids :   %d\n", plus->n_clines) ;
    prnmsg ("Number of faces     :   %d\n", plus->n_flines) ;
    prnmsg ("Number of kernels   :   %d\n", plus->n_klines) ;
    prnmsg ("Number of areas     :   %d\n", plus->n_areas) ;
    prnmsg ("Number of isles     :   %d\n", plus->n_isles) ;
    prnmsg ("Map is 3D           :   %d\n", Vect_is_3d(Map));

    return 1;
}

/*!
 \fn int Vect_save_topo ( struct Map_info *Map )
 \brief save topology file for vector map
 \return 1 on success, 0 on error
 \param Map_info structure
*/
int
Vect_save_topo ( struct Map_info *Map )
{
    struct Plus_head *plus ;
    char   fname[1024], buf[1024];
    FILE   *fp;
    
    G_debug (1, "Vect_save_topo()"); 

    plus = &(Map->plus);
    
    /*  write out all the accumulated info to the plus file  */
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    G__file_name (fname, buf, GV_TOPO_ELEMENT, Map->mapset);
    G_debug (1, "Open topo: %s", fname);
    fp = fopen( fname, "w");
    if ( fp ==  NULL) {
        G_warning("Can't open topo file for write: %s\n", fname);
	    return 0;
    }

    /* set portable info */
    dig_init_portable ( &(plus->port), dig__byte_order_out ());
    
    if ( 0 > dig_write_plus_file (fp, plus) ) {
        G_warning ("Error writing out topo file.\n");
	return 0;
    }
    
    fclose( fp );

    return 1;
}

/*!
 \fn int Vect_topo_dump ( struct Plus_head *plus, FILE *out )
 \brief dump topology to file
 \return 1 on success, 0 on error
 \param Map_info structure, file for output (stdout/stderr for example)
*/
int
Vect_topo_dump ( struct Plus_head *plus, FILE *out )
{
    int i, j, line, isle;
    P_NODE *Node;
    P_LINE *Line;
    P_AREA *Area;
    P_ISLE *Isle;
    BOUND_BOX box;

    fprintf (out, "---------- TOPOLOGY DUMP ----------\n" ); 
    
    /* box */
    Vect_box_copy ( &box, &(plus->box) );
    fprintf (out, "N,S,E,W,T,B: %f, %f, %f, %f, %f, %f\n", box.N, box.S, 
	                         box.E, box.W, box.T, box.B);
    
    /* nodes */
    fprintf (out, "Nodes (%d nodes, alive + dead ):\n", plus->n_nodes ); 
    for (i = 1; i <= plus->n_nodes; i++) {
	if ( plus->Node[i] == NULL ) { continue; }
	Node = plus->Node[i];
	fprintf (out, "node = %d, n_lines = %d, xy = %f, %f\n", i, Node->n_lines,
	                            Node->x, Node->y ); 
        for (j = 0; j < Node->n_lines; j++) {
	    line = Node->lines[j];
	    Line = plus->Line[abs(line)];
	    fprintf (out, "  line = %3d, type = %d, angle = %f\n", line, Line->type, Node->angles[j] ); 
	}
    }
    
    /* lines */
    fprintf (out, "Lines (%d lines, alive + dead ):\n", plus->n_lines ); 
    for (i = 1; i <= plus->n_lines; i++) {
	if ( plus->Line[i] == NULL ) { continue; }
	Line = plus->Line[i];
	fprintf (out, "line = %d, type = %d, offset = %ld n1 = %d, n2 = %d, "
	              "left/area = %d, right = %d\n",
		       i, Line->type, Line->offset, Line->N1, Line->N2,
	               Line->left, Line->right); 
	fprintf (out, "N,S,E,W,T,B: %f, %f, %f, %f, %f, %f\n", Line->N, Line->S,
	                       Line->E, Line->W, Line->T, Line->B);	
    }
    
    /* areas */
    fprintf (out, "Areas (%d areas, alive + dead ):\n", plus->n_areas ); 
    for (i = 1; i <= plus->n_areas; i++) {
	if ( plus->Area[i] == NULL ) { continue; }
	Area = plus->Area[i];
	
	fprintf (out, "area = %d, n_lines = %d, n_isles = %d centroid = %d\n", 
		 i, Area->n_lines, Area->n_isles, Area->centroid ); 
	
	fprintf (out, "N,S,E,W,T,B: %f, %f, %f, %f, %f, %f\n", Area->N, Area->S,
	                       Area->E, Area->W, Area->T, Area->B);	
		
        for (j = 0; j < Area->n_lines; j++) {
	    line = Area->lines[j];
	    Line = plus->Line[abs(line)];
	    fprintf (out, "  line = %3d\n", line ); 
	}
        for (j = 0; j < Area->n_isles; j++) {
	    isle = Area->isles[j];
	    fprintf (out, "  isle = %3d\n", isle ); 
	}
    }
    
    /* isles */
    fprintf (out, "Islands (%d islands, alive + dead ):\n", plus->n_isles ); 
    for (i = 1; i <= plus->n_isles; i++) {
	if ( plus->Isle[i] == NULL ) { continue; }
	Isle = plus->Isle[i];
	
	fprintf (out, "isle = %d, n_lines = %d area = %d\n", i, Isle->n_lines, 
		           Isle->area ); 
	
	fprintf (out, "N,S,E,W,T,B: %f, %f, %f, %f, %f, %f\n", Isle->N, Isle->S,
	                       Isle->E, Isle->W, Isle->T, Isle->B);	
	
        for (j = 0; j < Isle->n_lines; j++) {
	    line = Isle->lines[j];
	    Line = plus->Line[abs(line)];
	    fprintf (out, "  line = %3d\n", line ); 
	}
    }

    return 1;
}

/*!
 \fn int Vect_save_spatial_index ( struct Map_info *Map )
 \brief save spatial index file
 \return 1 on success, 0 on error
 \param Map_info structure
*/
int
Vect_save_spatial_index ( struct Map_info *Map )
{
    struct Plus_head *plus ;
    char   fname[1024], buf[1024];
    FILE   *fp;
    
    G_debug (1, "Vect_save_spatial_index()"); 

    plus = &(Map->plus);
    
    /*  write out rtrees to the sidx file  */
    sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, Map->name);
    G__file_name (fname, buf, GV_SIDX_ELEMENT, Map->mapset);
    G_debug (1, "Open sidx: %s", fname);
    fp = fopen( fname, "w");
    if ( fp ==  NULL) {
        G_warning("Can't open spatial index file for write: %s\n", fname);
	    return 0;
    }

    /* set portable info */
    dig_init_portable ( &(plus->spidx_port), dig__byte_order_out ());
    
    if ( 0 > dig_write_spidx (fp, plus) ) {
        G_warning ("Error writing out spatial index file.\n");
	return 0;
    }
    
    fclose( fp );

    return 1;
}

/*!
 \fn int Vect_spatial_index_dump ( struct Plus_head *plus, FILE *out )
 \brief dump spatial index to file
 \return 1 on success, 0 on error
 \param Map_info structure, file for output (stdout/stderr for example)
*/
int
Vect_spatial_index_dump ( struct Plus_head *plus, FILE *out ) 
{

    fprintf (out, "---------- SPATIAL INDEX DUMP ----------\n" ); 
    
    dig_dump_spidx ( out, plus); 

    return 1;
}
