#include <stdlib.h>
#include "gis.h"
#include "sqlp.h"

int
main (int argc, char **argv) 
{
    SQLPSTMT *st;
    char sql[5000];

    st = sqpInitStmt();
    
    
    while ( fgets ( sql, 5000, stdin) )
      {
        fprintf (stdout, "\nInput statement: %s", sql);
    
        st->stmt = sql;
        sqpInitParser(st);
    
        if ( yyparse() != 0 )
        {
            fprintf (stdout, "Error: statement was not parsed\n");
            sqpFreeStmt(st); 
            return (1);
        } 

        sqpPrintStmt(st);
      }
    sqpFreeStmt(st); 

    exit(0);
}

