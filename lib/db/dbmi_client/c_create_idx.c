#include "dbmi.h"
#include "macros.h"

/*!
 \fn int db_create_index (dbDriver *driver, dbIndex *index)
 \brief 
 \return 
 \param 
*/
int
db_create_index (dbDriver *driver, dbIndex *index)
{
    int ret_code;

/* start the procedure call */
    db__set_protocol_fds (driver->send, driver->recv);
    DB_START_PROCEDURE_CALL(DB_PROC_CREATE_INDEX);

/* send the arguments to the procedure */
    DB_SEND_INDEX (index);

/* get the return code for the procedure call */
    DB_RECV_RETURN_CODE(&ret_code);

    if (ret_code != DB_OK)
	return ret_code; /* ret_code SHOULD == DB_FAILED */

/* get results */
    DB_RECV_STRING(&index->indexName);

    return DB_OK;
}
