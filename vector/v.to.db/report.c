#include "dbmi.h"
#include "global.h"

int 
report (void)
{
    int i;
    char left[20], right[20];

    switch (options.option) {
        case O_CAT:
    	    fprintf (stdout,"cat\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d\n", Values[i].cat);
            break;	

        case O_COUNT:
    	    fprintf (stdout,"cat|count\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d|%d\n", Values[i].cat, Values[i].count1);
            break;

	case O_AREA:
    	    fprintf (stdout,"cat|area\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d|%.15g\n", Values[i].cat, Values[i].d1);	
            break;

        case O_LENGTH:
    	    fprintf (stdout,"cat|length\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d|%.15g\n", Values[i].cat, Values[i].d1);	        
            break;

        case O_COOR:
    	    fprintf (stdout,"cat|x|y|z\n");
	    for ( i = 0; i < vstat.rcat; i++ ) {
		if ( Values[i].count1 == 1 )
	            fprintf (stdout, "%d|%.15g|%.15g|%.15g\n", Values[i].cat, Values[i].d1, Values[i].d2, Values[i].d3);
	    }
            break;

        case O_SIDES:
    	    fprintf (stdout,"cat|left|right\n");
	    for ( i = 0; i < vstat.rcat; i++ ) {
		if ( Values[i].count1 == 1 ) {
		    if ( Values[i].i1 >= 0 )
		        sprintf ( left, "%d", Values[i].i1 );
		    else
		        sprintf ( left, "-1" ); /* NULL, no area/cat */
		} else if ( Values[i].count1 > 1 ) {
		    sprintf ( left, "-" );
		} else { /* Values[i].count1 == 0 */
		    G_warning ( "Bug in this module, nothing found on the left side of line with "
			        "category %d", Values[i].cat );
		    sprintf ( left, "e" );
		}

		if ( Values[i].count2 == 1 ) {
		    if ( Values[i].i2 >= 0 )
		        sprintf ( right, "%d", Values[i].i2 );
		    else
		        sprintf ( right, "-1" ); /* NULL, no area/cat */
		} else if ( Values[i].count2 > 1 ) {
		    sprintf ( right, "-" );
		} else { /* Values[i].count1 == 0 */
		    G_warning ( "Bug in this module, nothing found on the left side of line with "
			        "category %d", Values[i].cat );
		    sprintf ( right, "e" );
		}

	        fprintf ( stdout, "%d|%s|%s\n", Values[i].cat, left, right );
	    }
            break;

        case O_QUERY:
    	    fprintf (stdout,"cat|query\n");
	    for ( i = 0; i < vstat.rcat; i++ ) {
		if ( Values[i].null ) {
	            fprintf (stdout, "%d|-\n", Values[i].cat);
		} else {
		    switch ( vstat.qtype ) {
			case ( DB_C_TYPE_INT ):
	                    fprintf (stdout, "%d|%d\n", Values[i].cat, Values[i].i1);	        
			    break;
			case ( DB_C_TYPE_DOUBLE ):
	                    fprintf (stdout, "%d|%15g\n", Values[i].cat, Values[i].d1);       
			    break;
			case ( DB_C_TYPE_STRING ):
	                    fprintf (stdout, "%d|%s\n", Values[i].cat, Values[i].str1);	        
			    break;
		    }
		}
	    }
            break;
    } 

    return 0;
}

int 
print_stat (void)
{
    fprintf (stderr,"%d categories read from map\n", vstat.rcat);        
    fprintf (stderr,"%d records selected from table\n", vstat.select);        
    fprintf (stderr,"%d categories read from map exist in selection from table\n", vstat.exist);
    fprintf (stderr,"%d categories read from map don't exist in selection from table\n", vstat.notexist);    
    fprintf (stderr,"%d records updated/inserted\n", vstat.update);    
    fprintf (stderr,"%d update/insert errors\n", vstat.error);
    if ( vstat.dupl > 0 )
	fprintf (stderr,"%d categories with more points (coordinates not loaded)\n", vstat.dupl);        

    return 0;
}    
