/* SQLBASE wants to read an SQL.INI in the following places
 * (see p21 SQLBase for UNIX Installation Guide)
 *
 *  1. current directory 
 *  2. /SQLBASE (on current driver)
 *  3. Root directory (on current driver)  
 *  4. As specified in the directory search path
 *
 * NOTE: 2 and 3 may make no sense for Unix. They look like DOS methods
 *       4 is not specific enough. Do they mean in $PATH (hope not)
 *
 * Therefore this driver implements a new mechanism
 *    SQLBASE=directory
 *    SQLBASE_SERVER=server
 *
 * If SQLBASE is set, then the driver will cd to this directory
 *
 * NOTE2: It seems that teh SQLBASE software (and C/API) also look for
 *        $SQLBASE as a directory to look for SQL.INI
 *        
 *        There are other files in $SQLBASE that are needed
 *            MESSAGE.SQL and ERROR.SQL 
 *        If these files can't be located, C/API ets and prints
 *        message to stdout (OUCH!!!)
 */

#include "globals.h"

char *
get_sqlbase_directory()
{
    return (getenv(SQLBASE_DIR));
}

char *
get_sqlbase_server()
{
    return (getenv(SQLBASE_SERVER));
}

chdir_sqlbase()
{
    char *dir;
    char err[1024];

    if (dir = get_sqlbase_directory())
    {
	if (chdir (dir) != 0)
	{
	    sprintf (err, "$%s=%s", SQLBASE_DIR, dir);
	    db_syserror(err);
	    return DB_FAILED;
	}
    }
    return DB_OK;
}
