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

/* wshort(): write short integer, 2 bytes with little-endian ordering
 *  (least significant byte first) */
int
wshort( FILE *fp, int val )
{ 
    char out[2], *cval;
  
    cval = (char *) &val;
    
    if (endian == BO_LE)
      {
        out[0] = cval[0];
        out[1] = cval[1];
      }
    else
      {
        out[0] = cval[ sizeof(int) - 1 ];
        out[1] = cval[ sizeof(int) - 2 ];
      }
    
    fwrite (out, 2, 1, fp);
	      
    return (1);
}

/* wlong(): write long integer, 4 bytes with middle-endian byte ordering 
 * (as on the PDP-11). Thus if B0 is the least significant byte, 
 * and B3 is the most significant byte, 
 * DGN long ints are stored B2 B3 B0 B1 */
int
wlong( FILE *fp, int val )
{
    char out[4], *cval;

    cval = (char *) &val;

    if (endian == BO_LE)
      {
        out[0] = cval[2];
        out[1] = cval[3];
        out[2] = cval[0];
        out[3] = cval[1];
      }
    else
      {
        out[0] = cval[ sizeof(int) - 3 ];
        out[1] = cval[ sizeof(int) - 4 ];
        out[2] = cval[ sizeof(int) - 1 ];
        out[3] = cval[ sizeof(int) - 2 ];
      }
    
    fwrite (out, 4, 1, fp);
    
    return (1);
}

/* wtcb(): write TCB element */
int
wtcb( FILE *fp)
{
    char c;
	
    /* Because some parts of TCB element are unknown but important,
     * copy of example TCB is written to output an then some
     * known fields are overwritten */
    
    /* copy of template head */
    fwrite ( tcb, 1536, 1, fp);

    /* overwrite some known values */

    /* subunits per master unit */
    /* byte 1112 */
    fseek( fp, 1112, SEEK_SET);
    wlong ( fp, su ); 

    /* UOR per subunits */    
    wlong ( fp, uor ); 
    
    /* name of master units */
    fwrite ( "mu", 1, 2, fp);

    /* name of subunits */
    fwrite ( "su", 1, 2, fp);
    
    /* file is 2d */
    /* byte 1214 */
    c = 0;
    fseek( fp, 1214, SEEK_SET);
    fwrite ( &c, 1, 1, fp);
    
    /* go back to the end of the file */
    /* byte 1536 */
    fseek( fp, 1536, SEEK_SET);
    
    return (1);
}

/* wehead(): write element head (18 words = 36 bytes */ 
/* attr - TRUE if element contain attributes */
int
wehead( FILE *fp, EHEAD *head, int attr)
{
    int   i, wtf, coor;
    unsigned char  c;

    c = (char) head->level;  /* 1. half of word 1, bits 6,7 are ignored */
    if ( head->dgntype == DGN_CELL ) c = 0; 
    else if ( c < 1 || c > 63 ) c = 63;  

    if ( head->complex ) c |= 0x80;
    fwrite ( &c, 1, 1, fp);
    c = (char) head->dgntype;
    fwrite ( &c, 1, 1, fp);  /* 2. half of word 1, bit 8 is ignored */
  
    wtf = 16 + head->length;
    if ( attr ) wtf += head->attrlen;
    wshort (fp, wtf);         /* word 2 */
    
    /* word 3 - 14, bounding box */
    coor = ( int ) uor * su * head->W ;
    if ( coor >= 0 ) coor |= 0x80000000; else coor &= 0x7fffffff; 
    wlong (fp, coor);

    coor = ( int ) uor * su * head->S ;
    if ( coor >= 0 ) coor |= 0x80000000; else coor &= 0x7fffffff; 
    wlong (fp, coor);

    coor = 0;
    wlong (fp, coor);
    
    coor = ( int ) uor * su * head->E ;
    if ( coor >= 0 ) coor |= 0x80000000; else coor &= 0x7fffffff; 
    wlong (fp, coor);

    coor = ( int ) uor * su * head->N ;
    if ( coor >= 0 ) coor |= 0x80000000; else coor &= 0x7fffffff; 
    wlong (fp, coor);

    coor = 0;
    wlong (fp, coor);    
    
    /* word 15, graphic group */
    i = 0; 
    wshort ( fp, i ); 

    /* word 16 attindx */
    i = 2 + head->length;
    wshort ( fp, i );
    
    /* words 17, set attributes bit */    
    if ( attr )
        i = 0x0800; 
    else 
        i = 0;

    /* set snapable bit */    	
    if ( !head->snap ) 
        i |= 0x4000; 

    /* set hole bit */    	
    if ( head->hole ) 
        i |= 0x8000; 
	
    wshort ( fp, i);
    
    /* words 18 style, weight, color */    
    c = 0;
    fwrite ( &c, 1, 1, fp);
    c = (char) head->color;
    if ( head->color < 0 || head->color > 254 ) c = 254;  
    fwrite ( &c, 1, 1, fp);

    return 1;
}

/* wlink(): write linkage */
int
wmslink( FILE *fp, EHEAD *head )
{
    int  i;
    char out[4], *cval;
    
    /* linkage is read/write */
    i = 0;
    wshort ( fp, i);
    
    /* entitynum */
    i = entity;
    wshort ( fp, i);

    /* link */
    cval = (char *) &head->cat;

    if (endian == BO_LE)
      {
        out[0] = cval[0];
        out[1] = cval[1];
        out[2] = cval[2];
        out[3] = 0;
      }
    else
      {
        out[0] = cval[ sizeof(int) - 1 ];
        out[1] = cval[ sizeof(int) - 2 ];
        out[2] = cval[ sizeof(int) - 3 ];
        out[3] = 0;
      }
    
    fwrite (out, 4, 1, fp);
    
    return (1);
}
   
