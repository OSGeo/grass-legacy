#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "sqlp.h"

int
main (int argc, char **argv) 
{
    SQLPSTMT *st;
    char buf[5000], buf2[5000];
    dbString stmt;
    int len;
    

    st = sqpInitStmt();
    
    db_init_string (&stmt);

    while ( fgets (buf, 5000, stdin) ) {
	fprintf ( stdout, "Input row: -->>%s<<--\n", buf );
	strcpy ( buf2, buf );
	G_chop (buf2);
	len = strlen (buf2);
	
	if ( buf2[ len - 1 ] == ';' ) {  /* end of statement */
	    buf2 [len - 1] = 0;          /* truncate ';' */
	    db_append_string (&stmt, buf2);
            
	    st->stmt = db_get_string(&stmt);
            sqpInitParser(st);

	    fprintf ( stdout, "Input statement: -->>%s<<--\n", st->stmt );

	    if ( yyparse() != 0 )
	    {
		fprintf (stdout, "Error: statement was not parsed\n");
		sqpFreeStmt(st); 
		return (1);
	    } 

	    sqpPrintStmt(st);

	    db_zero_string (&stmt);

        } else { 
	    db_append_string (&stmt, buf);
	}
    }

    
    sqpFreeStmt(st); 

    exit(0);
}

