#include "dbmi.h"
#include "global.h"

int 
report (void)
{
    int i;

    switch (options.option) {
        case O_CAT:
    	    fprintf (stdout,"cat\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d\n", Values[i].cat);
            break;	

        case O_COUNT:
    	    fprintf (stdout,"cat|count\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d|%d\n", Values[i].cat, Values[i].i1);
            break;

	case O_AREA:
    	    fprintf (stdout,"cat|area\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d|%f\n", Values[i].cat, Values[i].d1);	
            break;

        case O_LENGTH:
    	    fprintf (stdout,"cat|length\n");
	    for ( i = 0; i < vstat.rcat; i++ )
	        fprintf (stdout, "%d|%f\n", Values[i].cat, Values[i].d1);	        
            break;

        case O_COOR:
    	    fprintf (stdout,"cat|x|y\n");
	    for ( i = 0; i < vstat.rcat; i++ ) {
		if ( Values[i].i1 == 1 )
	            fprintf (stdout, "%d|%f|%f\n", Values[i].cat, Values[i].d1, Values[i].d2);
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
	                    fprintf (stdout, "%d|%f\n", Values[i].cat, Values[i].d1);       
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
