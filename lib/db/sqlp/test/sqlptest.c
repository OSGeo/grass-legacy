#include "sqlp.h"
#include "gis.h"


main(argc, argv) char *argv[];
{
    SQLPSTMT *st;
    char sql[1024];

    st = sqpInitStmt();
    
    
    while ( fgets ( sql, 1023, stdin) )
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

