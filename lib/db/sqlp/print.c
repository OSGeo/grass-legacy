/*****************************************************************************
*
* MODULE:       SQL statement parser library 
*   	    	
* AUTHOR(S):    lex.l and yac.y were originaly taken from unixODBC and
*               probably written by Peter Harvey <pharvey@codebydesigns.com>,
*               modifications and other code by Radim Blazek
*
* PURPOSE:      Parse input string containing SQL statement to 
*               SQLPSTMT structure.
*               SQL parser may be used by simple database drivers. 
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include "sqlp.h"
#include <stdio.h>

int sqpPrintStmt(SQLPSTMT *st)
{
    int i;

    fprintf( stderr, "********** SQL PARSER RESULT **********\n" );
    fprintf( stderr, "INPUT: %s\n", sqlpStmt->stmt );
    fprintf( stderr, "COMMAND: ");
    switch ( sqlpStmt->command )
      {
	case (SQLP_CREATE):
            fprintf( stderr, "CREATE\n");
	    break;			
	case (SQLP_DROP):
            fprintf( stderr, "DROP\n");
	    break;			
	case (SQLP_INSERT):
            fprintf( stderr, "INSERT\n");
	    break;			
	case (SQLP_UPDATE):
            fprintf( stderr, "UPDATE\n");
	    break;			
	case (SQLP_SELECT):
            fprintf( stderr, "SELECT\n");
	    break;			
	case (SQLP_DELETE):
            fprintf( stderr, "DELETE\n");
	    break;			
	default:    
            fprintf( stderr, "UNKNOWN\n");
      }
	    
    fprintf( stderr, "TABLE: %s\n", sqlpStmt->table);
    
    /* columns */
    for (i=0; i < st->nCol; i++)
      {	    
	if ( sqlpStmt->command == SQLP_CREATE )
          {
            fprintf( stderr, "COLUMN %2d: ", i+1);
            switch ( sqlpStmt->ColType[i] )
              {
	        case (SQLP_VARCHAR):
                    fprintf( stderr, "type:varchar width:%d", sqlpStmt->ColWidth[i] );
	            break;			
	        case (SQLP_INTEGER):
                    fprintf( stderr, "type:integer" );
	            break;			
	        case (SQLP_DOUBLE):
                    fprintf( stderr, "type:double" );
	            break;			
	        case (SQLP_DATE):
                    fprintf( stderr, "type:date" );
	            break;			
	        default:
                    fprintf( stderr, "type:unknown" );
	            break;			
	      }
            fprintf( stderr, " name:%s\n", sqlpStmt->Col[i].s);
	  }
	else
	  {
            fprintf( stderr, "COLUMN %2d: %s\n", i+1, sqlpStmt->Col[i].s);
	  }
      }
    
    /* values */
    for (i=0; i < st->nVal; i++)
      {	    
        fprintf( stderr, "VALUE %2d ", i+1);
        switch ( sqlpStmt->Val[i].type )
          {
	    case (SQLP_S):
                fprintf( stderr, "(string) : %s\n", sqlpStmt->Val[i].s );
	        break;			
	    case (SQLP_I):
                fprintf( stderr, "(integer): %d\n", sqlpStmt->Val[i].i );
	        break;			
	    case (SQLP_D):
                fprintf( stderr, "(float)  : %f\n", sqlpStmt->Val[i].d );
	        break;			
            default:
                fprintf( stderr, "unknown\n" );
	        break;			
          }
      }
    
   fprintf( stderr, "***************************************\n" );

    return (1);
}
    

