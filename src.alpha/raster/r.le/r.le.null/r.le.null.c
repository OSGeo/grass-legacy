				/********************************/
				/*	r.le.null/r.le.null.c	*/
				/*				*/
				/*	Version 12/10/92	*/
				/*				*/
				/*  Programmers: Bucher, Baker	*/
				/*       Univ. of Wyoming	*/
				/********************************/


#include "gis.h"
#include "math.h"
#include "r.le.null.h"

struct CHOICE *choice ;
struct CAT *cats ;

main(argc, argv)
int   argc;
char  *argv[];
{
    register int     i, j ;
    int              ncats, rd,  prob, cat[25], nrows, ncols, fd, k;
    float            pos[25] ;
    struct Cell_head window;
    CELL            *buf, *buf1, *buf2 ;
    char             name[30] ;
    
				/* initialize the GRASS environment */ 
    
    G_gisinit(argv[0]);

				/* dynamically allocate storage for choice */

     
    choice = (struct CHOICE *) G_malloc(sizeof(struct CHOICE)) ;
    cats = (struct CAT *) G_malloc(sizeof(struct CAT)) ;

				/* get the user's input */

    user_input(argc,argv) ;

    printf("\nR.LE.NULL IS WORKING...\n");

    if( 0 > (fd = G_open_cell_new(choice->fn))) 
           G_fatal_error("Cannot open cell file '%s'\n", name);
	
    buf = G_allocate_cell_buf();
    buf1 = G_allocate_cell_buf();
    buf2 = G_allocate_cell_buf();
    G_zero_cell_buf(buf) ;
    nrows = G_window_rows();
    ncols = G_window_cols();

				/* for each pixel in the map */

    for(i=0; i<nrows; i+=3){
        for(j=0; j<ncols; j+=3){
           k = 0 ;
           prob = 0;

				/* get a random number */
	     
           rd = rand() % 10000 ;

				/* while the random number is greater than
				   the probability for this attribute */

	     do {
  		prob += (int)(*(cats->prob+k)*10000);
		k ++;
             }
    	     while(rd > prob && k < choice->ncats ) ;

				/* zero the cells in the map */

	     *(buf+j)    = 0;	   
             *(buf+j+1)  = 0;
	     *(buf+j+2)  = 0;

	     *(buf1+j)   = 0;
	     *(buf1+j+1) = 0;
	     *(buf1+j+2) = 0;

	     *(buf2+j)   = 0;
	     *(buf2+j+1) = 0;
	     *(buf2+j+2) = 0;

				/* if the random number is less than the
				   probability, then make this cell have
				   the attribute */

             if( rd <= prob ){
 	       *(buf+j)    = *(cats->att+k-1) ;
 	       *(buf+j+1)  = *(cats->att+k-1) ;
 	       *(buf+j+2)  = *(cats->att+k-1) ;

 	       *(buf1+j)   = *(cats->att+k-1) ;
 	       *(buf1+j+1) = *(cats->att+k-1) ;
 	       *(buf1+j+2) = *(cats->att+k-1) ;

	       *(buf2+j)   = *(cats->att+k-1) ;
 	       *(buf2+j+1) = *(cats->att+k-1) ;
 	       *(buf2+j+2) = *(cats->att+k-1) ;
             }
        }
        G_put_map_row( fd, buf) ;	         
        G_put_map_row( fd, buf1) ;
        G_put_map_row( fd, buf2) ;	  
    }
    free( buf ) ;
    free( buf1 ) ;
    free( buf2 ) ;
    free( choice ) ;
    G_close_cell(fd) ;
}
