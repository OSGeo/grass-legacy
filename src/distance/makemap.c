/* %W% %G% */

#include   "stdio.h"
#define ROWSIZE 300
#define COLSIZE 300
#define INIT     0
char   fname[]= "map.l" ;

char MAP[ROWSIZE][COLSIZE] ;

main (argc,argv) int argc ; char *argv[] ;
{
    int    r,c,mapsize ;
    FILE   *fp, *open_file() ;

    fp=open_file (fname,"w") ;

/* use to generate map-points

    for (r=0; r<ROWSIZE; r++)
    {   for (c=0; c<COLSIZE; c++)
            MAP[r][c]=INIT ;
    }
    MAP[150][150]=1 ;
*/

/* use to generate map with lines */

    for (r=0; r<ROWSIZE; r++) 
    {   c=  ((int)(.5 * r) + 3)%(COLSIZE-1) ;
        MAP[r][c]=1 ;

        c=  ((int)(.7 * r)+ 5)%(COLSIZE-1) ;
        MAP[r][c]=2 ;

        c=  ((2 * r)+ 1)%(COLSIZE-1) ;
        MAP[r][c]=3 ;

        c=  ((int)(4.5 * r)- 1)%(COLSIZE-1) ;
        MAP[r][c]=4 ;

        c=  (r- 1)%(COLSIZE-1) ;
        MAP[r][c]=5 ;

    }

/* use to generate area map

    for (r=14; r<29; r++)
    {    for (c=2; c<13; c++)
            MAP[r][c]=1 ;
    }

    for (r=21; r<37; r++)
    {    for (c=21; c<32; c++)
            MAP[r][c]=2 ;
    }

    for (r=26; r<39; r++)
    {    for (c=23; c<37; c++)
            MAP[r][c]=3 ;
    }

    for (r=46; r<50; r++)
    {    for (c=1; c<21; c++)
            MAP[r][c]=4 ;
    }

    for (r=6; r<19; r++)
    {    for (c=11; c<23; c++)
            MAP[r][c]=5 ;
    }

*/
    mapsize=ROWSIZE * COLSIZE ;

    writ_cbuf (fp, MAP, mapsize) ;
}
