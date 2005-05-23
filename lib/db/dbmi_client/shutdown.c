#include <stdlib.h>
#include <sys/wait.h>
#include "dbmi.h"

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
/* closedown the driver, and free the driver structure */
/* NOTE: the management of the memory for the driver structure
 * probably should be handled differently.
 *
 * db_start_driver() could take a pointer to driver structure as
 * an argument, instead of returning the pointer to allocated
 * then there would be no hidden free required
 */
db_shutdown_driver (driver)
    dbDriver *driver;
{
    int pid;
    int status;

/* close the communication FILEs */
    fclose (driver->send);
    fclose (driver->recv);

    driver->send = NULL;
    driver->recv = NULL;

/* wait for the driver to finish */
    status = -1;
    while ((pid = wait(&status)) > 0 && pid != driver->pid)
	 {}

    driver->pid = 0;

/* free the driver structure. THIS IS GOOFY */
    free (driver);

    return status;
}
