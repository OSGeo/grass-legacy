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

#include "string.h"
#include "gis.h"
#include "Vect.h"
#include "dgn.h"
#include "tcb.h"
#include "proto.h"

/* write points */
int
wpoint( FILE *fp, EHEAD *head, int wlink)
{
    int i, x, y;
    unsigned char  c;
    
    head->dgntype = DGN_LINE;
    head->complex = FALSE;
    head->length = 8;
    if ( wlink ) head->length += 4; 
 
    wehead ( fp, head, wlink );

    x = (int) uor * su * head->points->x[0];
    y = (int) uor * su * head->points->y[0];
    for (i = 0; i < 2; i++)
      {
        wlong (fp, x);
        wlong (fp, y);
      }

    if ( wlink ) wmslink( fp, head );

    return 1;
}

/* Write parts of complex element, no attributes are written. */
/* opt: PARTS_NUMBER -  function returnes number of elements only
        PARTS_LENGTH -  function returnes only length of elements
                        in 2-byte words 
        PARTS_WRITE  - write */
int
wparts( FILE *fp, EHEAD *head, int opt )
{
    int i, j, l, nparts, rest, length;
    int x, y, np, wnp , fpnt, lpnt;
    unsigned char  c;
    
    np = head->points->n_points;
    /* A maximum of MAX_LINE_PNTS vertices can be in line */

    /* how many parts split the line to */
    nparts = ( np - 1 ) / ( MAX_LINE_PNTS - 1 );
    rest = ( np - 1 ) % ( MAX_LINE_PNTS - 1 );
    if ( rest > 0 ) nparts++;

    if ( opt == PARTS_NUMBER ) return (nparts);

    /* length in 2-byte words */
    length = nparts * (18 + 1) + (nparts - 1) * MAX_LINE_PNTS * 4 + (rest+1) * 4;
		    /* 18 + 1 = head + number_of_points */

    if ( opt == PARTS_LENGTH ) return (length);
	       
    head->dgntype = DGN_LINESTRING;  
    head->complex = TRUE;

    lpnt = 0;
    while ( lpnt < ( np - 1 ) )
      {
        fpnt = lpnt;
        if ( np > ( lpnt + MAX_LINE_PNTS - 1 ) )
            lpnt += MAX_LINE_PNTS - 1;
	else
	    lpnt = np - 1;
		   
        wnp = lpnt - fpnt + 1;
	head->length = 1 + 4 * wnp;
        head->attrlen = 0;
             
	wehead ( fp, head, FALSE);
        wshort (fp, wnp);

        for (i = fpnt; i <= lpnt; i++)
	  {
            x = (int) uor * su * head->points->x[i];
            y = (int) uor * su * head->points->y[i];
            wlong (fp, x);
            wlong (fp, y);
          }
       }

    return 1;
}

/* write line */
/* complex must be TRUE if line is part of complex element */
int
wline( FILE *fp, EHEAD *head, int wlink, int complex )
{
    int i, j, l, nparts, partslen;
    int x, y, np, wnp , fpnt, lpnt;
    unsigned char  c;
    
    np = head->points->n_points;

    nparts = wparts ( fp, head, PARTS_NUMBER);
    partslen = wparts ( fp, head, PARTS_LENGTH);

    if ( nparts == 1 ) 
      {
         head->dgntype = DGN_LINESTRING;  
         head->complex = FALSE;
	 if ( complex ) head->complex = TRUE;
         head->length = 1 + np * 4;
         head->attrlen = 4;		 
                 
         wehead ( fp, head, wlink);
         wshort (fp, np);

         for (i = 0; i < np; i++)
           {
             x = (int) uor * su * head->points->x[i];
             y = (int) uor * su * head->points->y[i];
             wlong (fp, x);
             wlong (fp, y);
           }

	 if ( wlink ) wmslink( fp, head );
       }
     else 
       { 
         /* write comlex chain */
	 head->dgntype = DGN_COMPLEX_STRING;
	 head->complex = FALSE;
	 if ( complex ) head->complex = TRUE;
	 head->length = 2;
         head->attrlen = 4;		 

         wehead ( fp, head, TRUE);
                 
	 /* total length in words = rest of header + partslen */ 
	 i = 5 + partslen;
         wshort (fp, i); 
		 
	 /* # of elements in complex */
	 wshort (fp, nparts); 
		 
         if ( wlink ) 
            wmslink( fp, head );
	 else
	   {
	     /* write empty attributes */
	     i = 0;
	     for ( j = 0; j < 4; j++)
	       wshort ( fp, i); 
	   }

          wparts ( fp, head, PARTS_WRITE);

          slines++;
       }

    return (0);
}

/* write area */
int
warea( FILE *fp, EHEAD *head, int wlink, int complex)
{
    int i, j, l, nparts, partslen;
    int x, y, np, wnp , fpnt, lpnt;
    unsigned char  c;

    np = head->points->n_points;
    
    nparts = wparts ( fp, head, PARTS_NUMBER);
    partslen = wparts ( fp, head, PARTS_LENGTH);

    if ( nparts == 1 ) 
      {
         head->dgntype = DGN_SHAPE;  
         head->complex = FALSE;
	 if ( complex ) head->complex = TRUE;
         head->length = 1 + np * 4;
         head->attrlen = 8; /* fill color */
         if ( wlink ) head->attrlen += 4;

	 wehead ( fp, head, TRUE); /* because always fill color attrib */
         wshort (fp, np);

         for (i = 0; i < np; i++)
           {
             x = (int) uor * su * head->points->x[i];
             y = (int) uor * su * head->points->y[i];
             wlong (fp, x);
             wlong (fp, y);
           }

         /* fill information */
	 wshort( fp,  0x1007);
	 wshort( fp,  0x0041);
	 wshort( fp,  0x0802);
	 wshort( fp,  0x0001);		  
	 c = (char) head->color;
	 if ( head->color < 0 || head->color > 254 ) c = 254;
	     fwrite ( &c, 1, 1, fp); 
	     
	 c = 0;		 
	 for (i = 0; i < 7; i++)  fwrite ( &c, 1, 1, fp); 

	 if ( wlink ) wmslink( fp, head );
       }
     else 
       { 
         /* write comlex shape */
	 head->dgntype = DGN_COMPLEX_SHAPE;
	 head->complex = FALSE;
	 if ( complex ) head->complex = TRUE;
	 head->length = 2;
         head->attrlen = 8; /* fill color */
         if ( wlink ) head->attrlen += 4;

         wehead ( fp, head, TRUE);
                 
	 /* total length in words = rest of header + partslen */ 
	 i = 5 + partslen;
         wshort (fp, i); 
		 
	 /* # of elements in complex */
	 wshort (fp, nparts); 
		 
         /* fill information */
	 wshort( fp,  0x1007);
	 wshort( fp,  0x0041);
	 wshort( fp,  0x0802);
	 wshort( fp,  0x0001);		  
	 c = (char) head->color;
	 if ( head->color < 0 || head->color > 254 ) c = 254;
	     fwrite ( &c, 1, 1, fp); 
	     
	 c = 0;		 
	 for (i = 0; i < 7; i++)  fwrite ( &c, 1, 1, fp); 	 
	 
         if ( wlink ) wmslink( fp, head );

         wparts ( fp, head, PARTS_WRITE);

         slines++;
       }

    return 1;
}

/* write cell header */
int
wcell( FILE *fp, EHEAD *head, int wlink, int length, double x , double y)
{
    int i, j, l, coor;
    int np, wnp , fpnt, lpnt;
    unsigned char  c;

    head->dgntype = DGN_CELL;  
    head->complex = FALSE;
    head->length = 28;
    if ( wlink ) head->attrlen += 0;

    wehead ( fp, head, wlink);
    
    /* total length in words = rest of header + length of parts ???*/
    i = 27 + length;
    wshort (fp, i); 
    
    /* Radix 50 name */
    i = 0; 
    wlong (fp, i); 

    /* class bit map */
    i = 1; 
    wshort (fp, i); 

    /* levels used in cell */
    for ( j =0 ; j < 4 ; j++)
      {
	i = 0xffff; 
	wshort (fp, i); 
      }
      
    /* range block low */
    coor = ( int ) uor * su * head->W ;
    wlong (fp, coor);        

    coor = ( int ) uor * su * head->S ;
    wlong (fp, coor);

    /* range block high */
    coor = ( int ) uor * su * head->E ;
    wlong (fp, coor);
      
    coor = ( int ) uor * su * head->N ;
    wlong (fp, coor);

    /* transformation matrix */
    i = 1000000 / 4.6566 ; 
    wlong (fp, i); 
    i = 0; 
    wlong (fp, i); 
    i = 0; 
    wlong (fp, i); 
    i = 1000000 / 4.6566 ;
    wlong (fp, i); 


    /* cell origin */
    coor = ( int ) uor * su * x ;
    wlong (fp, coor);

    coor = ( int ) uor * su * y ;
    wlong (fp, coor);

    return 1;
}


/* write text */
int
wtext( FILE *fp, EHEAD *head, char *t, double xd, double yd )
{
    int i, size, tlen;
    int x,y;
    char c, txt[21];

    size = 5;

    strncpy ( txt, t, 20);
    tlen = (char) strlen (txt);

    x = (int) uor * su * xd;
    y = (int) uor * su * yd;

    head->dgntype = DGN_TEXT;
    head->complex = FALSE;
    head->length = 12 + 10;   /* is text header and text */ 

    head->N = yd + size;
    head->S = yd;
    head->E = xd + size * tlen;
    head->W = xd;

    wehead ( fp, head, FALSE);
    
    /* font number */    
    c = 1;
    fwrite ( &c, 1, 1, fp);    
    
    /* justification type */
    c = 2; /* left bottom */
    fwrite ( &c, 1, 1, fp);    
    
    /* length multiplier */
    i = (int) 1000 * size * uor * su / 6 ; 
    wlong (fp, i);    

    /* height multiplier */
    i = (int) 1000 * size *  uor * su / 6 ; 
    wlong (fp, i);    

    /* rotation angle */
    i = 0; 
    wlong (fp, i);    

    /* origine */
    wlong (fp, x);
    wlong (fp, y);

    /* # of characters */
    c = 20;
    fwrite ( &c, 1, 1, fp);    

    /* # of enter data fields */
    c = 1;
    fwrite ( &c, 1, 1, fp);    

    fwrite ( txt, 20, 1, fp);    
    
    return 1;
}

