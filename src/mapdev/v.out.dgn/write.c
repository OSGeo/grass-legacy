/*****************************************************************************
 *
 * MODULE:       v.out.dgn 
 * AUTHOR(S):    Radim Blazek
 * PURPOSE:      Export GRASS vector file to Microstation DGN file
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include "gis.h"
#include "Vect.h"
#include "dgn.h"
#include "proto.h"

int
write_dgn(char *name, char *mapset, char *dgn)
{
    struct Map_info map;	
    FILE   *fp;
    int    elem, island, nisles, wlink, npa, nparts, partslen; 
    int    i, j, typ, Num, complex;
    struct line_pnts *Points;
    EHEAD  head;
    double x, y;
    char   txt[20];

    if(Vect_open_old(&map, name, mapset) != 2)
        G_fatal_error("Vector file was not opened at level 2.");
    
    if ( append )
        fp = fopen ( dgn, "r+");
    else
        fp = fopen ( dgn, "w");
    
    if( fp == NULL)
      {
        Vect_close(&map);
        G_fatal_error("Cannot open dgn file.");
      } 
      
    if ( append )
      fseek( fp, -2, SEEK_END);
    else
       wtcb ( fp );

    Points = Vect_new_line_struct();  
    head.level = level;
    head.color = color;
    head.points = Points;
    head.hole = FALSE;
    head.snap = TRUE;

    /* write area centroids */  
    if ( centroid )
      {
        Num = V2_num_areas(&map);
        for (elem = 1; elem <= Num; elem++)
	  {
	    head.cat = V2_area_att( &map, elem);
	    if (cat > -1 && cat != head.cat ) continue;
            if ( (head.cat > 0) && (catas & CAT_MSLINK) ) 
	        wlink = TRUE; 
	    else 
	        wlink = FALSE;
		
            if ( catas & CAT_LEVEL ) head.level = head.cat;
            if ( catas & CAT_COLOR ) head.color = head.cat;

	    typ = Vect_get_area_points(&map, elem, Points);
	    if(typ==-1) G_fatal_error("Out of Memory.\n");
	    if(typ==-2) return (0);
            V2_get_area_bbox ( &map, elem, &head.N, &head.S, &head.E, &head.W);	    

            if ( (head.cat > 0 ) && (catas & CAT_TEXT) )
	      {
                sprintf ( txt, "%d", head.cat);
		x = map.Att[map.Area[elem].att].x;
		y = map.Att[map.Area[elem].att].y;		
                wtext ( fp, &head, txt, x, y);
	      }	    
	  }
      
      }


    /* write areas as shapes */  
    if ( area )
      {
        Num = V2_num_areas(&map);
        for (elem = 1; elem <= Num; elem++)
	  {
	    head.cat = V2_area_att( &map, elem);
	    if (cat > -1 && cat != head.cat ) continue;

	    complex = FALSE;
	    nisles = map.Area[elem].n_isles;
	    if ( nisles > 0 )
	      {
		/* calculate length of all parts without mslink attributes 
		   but wiht fill color */
		/* area */   
		typ = Vect_get_area_points(&map, elem, Points);
		npa = wparts ( fp, &head, PARTS_NUMBER);
		partslen = wparts ( fp, &head, PARTS_LENGTH);
		nparts = npa;
		partslen += npa * 8;

        	V2_get_area_bbox ( &map, elem, &head.N, &head.S, &head.E, &head.W);	    
		for ( i=0; i < nisles; i++)
    	    	  {
        	    island = map.Area[elem].isles[i];
        	    typ = Vect_get_isle_points(&map, island, Points);
		    npa = wparts ( fp, &head, PARTS_NUMBER);
		    nparts += npa;
		    partslen += wparts ( fp, &head, PARTS_LENGTH);
		    /* add space for fill color */
		    partslen += npa * 8;
		  }

		wlink = FALSE;
		head.hole = TRUE;
		x = Points->x[0];
		y = Points->y[0];
		head.snap = FALSE;
		head.level = 0;
		wcell( fp, &head, wlink, partslen, x, y);
		head.level = level;
		head.color = color;	    
		complex = TRUE; /* area and islands are complex - parts of cell */
	      }

	    /* write area */
	    head.hole = FALSE;
	    head.snap = TRUE;
	    head.cat = V2_area_att( &map, elem);
            if ( (head.cat > 0) && (catas & CAT_MSLINK) ) 
	        wlink = TRUE; 
	    else 
	        wlink = FALSE;
		
            if ( catas & CAT_LEVEL ) head.level = head.cat;
            if ( catas & CAT_COLOR ) head.color = head.cat;

	    typ = Vect_get_area_points(&map, elem, Points);
	    if(typ==-1) G_fatal_error("Out of Memory.\n");
	    if(typ==-2) return (0);

    	    V2_get_area_bbox ( &map, elem, &head.N, &head.S, &head.E, &head.W);	    

            warea( fp, &head, wlink, complex );	

	    /* write islands */
	    head.hole = TRUE;
	    for ( i=0; i < nisles; i++)
    	      {
        	island = map.Area[elem].isles[i];
        	typ = Vect_get_isle_points(&map, island, Points);
		if(typ==-1) G_fatal_error("Out of Memory.\n");
		if(typ==-2) return (0);

        	V2_get_area_bbox ( &map, i, &head.N, &head.S, &head.E, &head.W);
		wlink = FALSE;
        	warea( fp, &head, wlink, TRUE );
	      }

            if ( (head.cat > 0 ) && (catas & CAT_TEXT) )
	      {
                sprintf ( txt, "%d", head.cat);
		x = map.Att[map.Area[elem].att].x;
		y = map.Att[map.Area[elem].att].y;		
                wtext ( fp, &head, txt, x, y);
	      }	    	      
	      
	  }
      
      }

    /* write points, lines and boundaries */  
    Num = V2_num_lines(&map);
    for (elem = 1; elem <= Num; elem++)
      {
	    head.cat = V2_line_att(&map, elem);
	    if (cat > -1 && cat != head.cat ) continue;	    
            if ( (head.cat > 0) && (catas & CAT_MSLINK) ) 
	        wlink = TRUE; 
	    else
	        wlink = FALSE;

            sprintf ( txt, "%d", head.cat);
		
            if ( catas & CAT_LEVEL ) head.level = head.cat;
            if ( catas & CAT_COLOR ) head.color = head.cat;
	    
	    typ = V2_read_line(&map, Points, elem);
	    if(typ==-1) G_fatal_error("Out of Memory.\n");
	    if(typ==-2) return(0);
            V2_get_line_bbox ( &map, elem, &head.N, &head.S, &head.E, &head.W);

            if ( typ == LINE ) nlines++;

            if ( (gtype & DOT) && typ == DOT)
	      {
	        wpoint( fp, &head, wlink );
		if ( (head.cat > 0 ) && (catas & CAT_TEXT) )
	          {
		    x = Points->x[0];
		    y = Points->y[0];		    
                    wtext ( fp, &head, txt, x, y);
	          }
	      }            
	    else if ( (gtype & LINE) && typ == LINE )
	      {
	        wline( fp, &head, wlink, FALSE );
		if ( (head.cat > 0 ) && (catas & CAT_TEXT) )
	          {
		    line_cent ( Points, &x, &y);
                    wtext ( fp, &head, txt, x, y);
	          }
	      }
	    else if ( (gtype & AREA) && typ == AREA )
	      {
	        wline( fp, &head, FALSE, FALSE );		
              }
      }


    Vect_destroy_line_struct(Points);
    Vect_close(&map);

    /* end of file */
    i = 0xFFFF;
    wshort(fp, i);
    
    fclose (fp);
    return (0);
}

