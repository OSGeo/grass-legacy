#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "conv.h"
#include "local_proto.h"

int 
old2new (char *in, char *out, int endian)
{
    int    i, j, nlines, ncats, sline, att;
    char   *mapset;
    FILE   *Digin, *Attin;
    struct Line *lines;  /* array of points and lines */
    struct Categ *cats;  /* array of categories */
    struct Map_info Mapout;
    double dist, sdist;
    struct line_cats *cat_out;
    struct line_pnts *pnt_out;
    
    /* open in map */
    if ( NULL == (mapset = G_find_file ("dig", in, "") )) {
	fprintf(stderr,"Input vector was not found.\n") ;
	exit(-1);
    }

    /* open input dig file */
    if ((Digin  = G_fopen_old ("dig", in, mapset)) == NULL ) {
        fprintf(stderr,"Failed openning input dig file.\n") ;
        exit(-1);
    } 

    /* open new output file */
    if ( Vect_open_new ( &Mapout, out, WITHOUT_Z) < 0) {
        fprintf(stderr,"Failed openning output dig file.\n") ;
	fclose (Digin);
    	exit(-1);
    }  
    
    /* open input dig_att file if exists */
    att = FALSE;
    if (NULL == (mapset = G_find_file ("dig_att", in, ""))) {
    	fprintf(stderr,"dig_att file doesn't exist.\n"); 
    } else {
        if (NULL == (Attin = G_fopen_old ("dig_att", in, mapset))) {
    	    fprintf(stderr,"Failed openning input dig_att file.\n"); 
	} else {
	   att = TRUE;
	}
   
    /* read old dig file */
    nlines = read_dig ( Digin, &Mapout, &lines, endian, att );

    /* read old dig_att file */
    ncats = 0;
    if ( att ) {
            ncats = read_att (Attin, &cats );
            fclose (Attin);
        }
    }
     	
    /* Attach categories to lines and points.
     * Walk through all cats and always find nearest line.
     * If cat is already attached but new one is nearer
     * set cat to new one.  */
     
     fprintf(stdout,"Attaching categories... ");
     
     for (i=0; i < ncats; i++){
         if ( cats[i].type & (DOT | LINE) ) {
		sline = -1;
                for (j=0; j < nlines; j++){
                    if (lines[j].type == cats[i].type) {
	                dist = ldist ( cats[i].x, cats[i].y, 
				       &(lines[j]));
			if (sline == -1 || dist < sdist){
			    sline = j;
			    sdist = dist;
			}
		    }
	        }
		if ( sline == -1 ) {
                    fprintf (stderr, "Failed to attach an attribute (category %d) to a line.\n", cats[i].cat);
		} else {
	            if ( lines[sline].cat > -1 ) {
                        fprintf (stderr, "WARNING: line %d label: %d matched another label: %d.\n", sline, lines[sline].cat, cats[i].cat);
		    }
	            lines[sline].cat = cats[i].cat;
		}
	}
    }
    fprintf(stdout,"Done.\n");
     
    /* Write to new file */
    fprintf(stdout,"Writting new file...\n");
    pnt_out = Vect_new_line_struct ();
    cat_out = Vect_new_cats_struct();
    
    j = 0;
    /* Write all points and lines if dig_att exists */
    if (att) {
        for (i=0; i < nlines; i++) {
            if ( lines[i].cat > 0 ) {
	        Vect_cat_set ( cat_out, 1, lines[i].cat );
	    }
            dig_alloc_points ( pnt_out, lines[i].n_points );
	    memcpy ( (void *) pnt_out->x, (void *) lines[i].x,
		     lines[i].n_points * sizeof(double)); 
	    memcpy ( (void *) pnt_out->y, (void *) lines[i].y,
		     lines[i].n_points * sizeof(double)); 
	    pnt_out->n_points = lines[i].n_points;
            Vect_write_line ( &Mapout, lines[i].type, pnt_out, cat_out );
	    j++;
            Vect_reset_cats (cat_out);
        }
        fprintf(stdout,"%-5d points and lines written to output file.\n",j);
    }
    /* Write centroids */    
    j = 0;
    for (i=0; i < ncats; i++) {
        if ( cats[i].type == CENTROID ){
            Vect_append_point ( pnt_out, cats[i].x, cats[i].y); 
	    Vect_cat_set ( cat_out, 1, cats[i].cat );
            Vect_write_line ( &Mapout, CENTROID, pnt_out, cat_out );
	    j++;
	    Vect_reset_line (pnt_out);
            Vect_reset_cats (cat_out);
	}
    } 
    fprintf(stdout,"%-5d centroids written to output file.\n",j);
    
    /* free memory */
    for (i=0; i < nlines; i++) {
	free (lines[i].x);    
	free (lines[i].y);    
    }
    free (lines);
    free (cats);
    Vect_destroy_cats_struct (cat_out);
    Vect_destroy_line_struct (pnt_out);
    
    return (1);
}


