#include "global.h"

int 
report (void)
{
    int i;

    switch (options.option) {
        case O_CAT:
    	    fprintf (stdout,"cat\n");
	    for ( i = 0; i < stat.rcat; i++ )
	        fprintf (stdout, "%d\n", list_ci[i].cat);
            break;	

        case O_COUNT:
    	    fprintf (stdout,"cat|count\n");
	    for ( i = 0; i < stat.rcat; i++ )
	        fprintf (stdout, "%d|%d\n", list_ci[i].cat, list_ci[i].i1);
            break;

        case O_LABEL:
    	    fprintf (stdout,"cat|label\n");
	    for ( i = 0; i < stat.rcat; i++ )
	        fprintf (stdout, "%d|%s\n", list_cc[i].cat, list_cc[i].c1);
            break;
	
	case O_AREA:
    	    fprintf (stdout,"cat|area\n");
	    for ( i = 0; i < stat.rcat; i++ )
	        fprintf (stdout, "%d|%f\n", list_cd[i].cat, list_cd[i].d1);	
            break;

        case O_LENGTH:
    	    fprintf (stdout,"cat|length\n");
	    for ( i = 0; i < stat.rcat; i++ )
	        fprintf (stdout, "%d|%f\n", list_cd[i].cat, list_cd[i].d1);	        
            break;

        case O_COOR:
    	    fprintf (stdout,"cat|x|y\n");
	    for ( i = 0; i < stat.rcat; i++ )
	        fprintf (stdout, "%d|%f|%f\n", list_ci2d[i].cat, list_ci2d[i].d1, list_ci2d[i].d2);	        	
            break;
    } 


    return OK;
}

int 
print_stat (void)
{
    fprintf (stderr,"\n%d categories in map\n", stat.cat);    
    fprintf (stderr,"%d categories read\n", stat.rcat);        
    fprintf (stderr,"%d records selected from table\n", stat.select);        
    fprintf (stderr,"%d categories from map exist in selection from table\n", stat.exist);
    fprintf (stderr,"%d categories from map do not exist in selection from table\n", stat.notexist);    
    fprintf (stderr,"%d records updated/inserted\n", stat.update);    
    fprintf (stderr,"%d update/insert errors\n", stat.error);
    if ( stat.dupl > 0 )
	fprintf (stderr,"%d categories with more elements not loaded\n", stat.dupl);        
}    