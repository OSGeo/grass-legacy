#include "dbmi.h"

/*!
 \fn char *db_list_drivers(void)
 \brief return comma separated list of existing DB drivers, used for driver parameter options
 \return return char
 \param void
*/
char *
db_list_drivers(void)
{
    dbDbmscap *list, *cur;
    dbString dbstring;

    db_init_string(&dbstring);

    /* read the dbmscap info */
    if(NULL == (list = db_read_dbmscap()))
	return NULL;
    else
    {
        /* build the comma separated string of existing drivers */
	for (cur = list; cur; cur = cur->next)
	{
	    if (cur->driverName == '\0')
	       break;
	    else
	    {
	       if(cur != list)
	          db_append_string ( &dbstring, ",");
	       db_append_string ( &dbstring, cur->driverName);
	    }
	}
    }
    
    return db_get_string (&dbstring);
}
