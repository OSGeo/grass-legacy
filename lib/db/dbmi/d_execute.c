#include "dbmi.h"
#include "macros.h"

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_d_execute_immediate()
{
    int stat;
    dbString SQLstatement;

/* get the arg(s) */
    db_init_string (&SQLstatement);
    DB_RECV_STRING(&SQLstatement);

/* call the procedure */
    stat = db_driver_execute_immediate (&SQLstatement);
    db_free_string (&SQLstatement);

/* send the return code */
    if (stat != DB_OK)
    {
	DB_SEND_FAILURE();
	return DB_OK;
    }
    DB_SEND_SUCCESS();

/* no results */
    return DB_OK;
}
