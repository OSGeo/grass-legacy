#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"


int label (
    struct Map_info *Map, int type, int do_area, 
    struct cat_list *Clist, LATTR *lattr)
{
    int i, ltype;
    double xl, yl;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int X, Y, T, B, L, R, Xoffset, Yoffset, xarr[5], yarr[5];
    int cat;
    char text[50];
    
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    Vect_rewind ( Map );

    while (1)
    {
	ltype =  Vect_read_next_line (Map, Points, Cats);   
        switch ( ltype )
	{
	case -1:
	    fprintf (stderr, "\nERROR: vector file - can't read\n" );
	    return -1;
	case -2: /* EOF */
	    return  0;
	}

	if ( !(type & ltype) ) continue; /* used for both lines and labels */
	
	R_standard_color(lattr->color) ;
	R_text_size(lattr->size, lattr->size) ;
	R_font(lattr->font) ;
		  
        if ( Clist->n_ranges > 0)
          {
	    Vect_cat_get(Cats, Clist->field, &cat);
	    if ( !(Vect_cat_in_cat_list (cat, Clist)) )
	         continue;
	  }
	
	if( Vect_cat_get(Cats, lattr->field, &cat) )
	  {	    
	    if ( (ltype & ELEMENT_TYPE_DOT) || Points->n_points == 1 )
	      /* point/centroid or line/boundary with one coor */     
	      {
	        X = (int)(D_u_to_d_col(Points->x[0])) ;
	        Y = (int)(D_u_to_d_row(Points->y[0])) ;
	      }
	    else if ( Points->n_points == 2 ) /* line with two coors */
	      {
                xl = (Points->x[0] + Points->x[1]) / 2;
                yl = (Points->y[0] + Points->y[1]) / 2;
	        X = (int)(D_u_to_d_col(xl)) ;
	        Y = (int)(D_u_to_d_row(yl)) ;
	      }
	    else
	      {
                i = Points->n_points / 2;   
	        X = (int)(D_u_to_d_col(Points->x[i])) ;
	        Y = (int)(D_u_to_d_row(Points->y[i])) ;
	      }
	   
            X = X + 0.5 * lattr->size;
            Y = Y + 1.5 * lattr->size;
	
            R_move_abs(X, Y) ;
	    sprintf (text, "%d", cat);
            R_get_text_box(text, &T, &B, &L, &R);
		
            /* Expand border 1/2 of text size */
	    T = T - lattr->size / 2 ;
	    B = B + lattr->size / 2 ;
	    L = L - lattr->size / 2 ;
            R = R + lattr->size / 2 ;
               
	    Xoffset = 0;
	    Yoffset = 0;
	    if (lattr->xref == LCENTER) Xoffset = -(R - L) / 2 ;
	    if (lattr->xref == LRIGHT ) Xoffset = -(R - L) ;
            if (lattr->yref == LCENTER) Yoffset = -(B - T) / 2 ;
	    if (lattr->yref == LBOTTOM) Yoffset = -(B - T) ; 
		
	    if ( lattr->bgcolor || lattr->bcolor )
	      {
	        xarr[0] = xarr[1] = xarr[4] = L + Xoffset; 
	        xarr[2] = xarr[3] = R + Xoffset; 
	        yarr[0] = yarr[3] = yarr[4] = B + Yoffset; 
	        yarr[1] = yarr[2] = T + Yoffset; 
		
                if( lattr->bgcolor)
                  {
	            R_standard_color( lattr->bgcolor) ;
		    R_polygon_abs(xarr, yarr, 5) ;
                  }
		
                if( lattr->bcolor)
	          {
	             R_standard_color( lattr->bcolor) ;
		     R_polyline_abs(xarr, yarr, 5) ;
	          }
	        R_standard_color(lattr->color) ;
	      }
		
	    R_move_abs(X + Xoffset, Y + Yoffset) ;
  	    R_text(text);
	}
    }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0;
}



