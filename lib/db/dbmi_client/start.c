#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "dbmi.h"

#define READ  0
#define WRITE 1

extern char *getenv();

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
dbDriver *
db_start_driver(name)
    char *name;
{
    dbDriver *driver;
    dbDbmscap *list, *cur;
    char *startup;
    int p1[2], p2[2];
    int pid;
    int stat;
    dbConnection connection;

/* read the dbmscap file */
    if(NULL == (list = db_read_dbmscap()))
	return (dbDriver *) NULL;

/* if name is empty use connection.driverName, added by RB 4/2000 */
    if( name == '\0' )
    {
	db_get_connection( &connection );
	if(NULL == (name = connection.driverName) )
	   return (dbDriver *) NULL;
    }

/* find this system name */
    for (cur = list; cur; cur = cur->next)
	if (strcmp (cur->driverName, name) == 0)
	    break;
    if (cur == NULL)
    {
	char msg[256];

	db_free_dbmscap (list);
	sprintf (msg, "%s: no such driver available", name );
	db_error (msg);
	return (dbDriver *) NULL;
    }

/* allocate a driver structure */
    driver = (dbDriver *) db_malloc (sizeof(dbDriver));
    if (driver == NULL)
    {
	db_free_dbmscap (list);
	return (dbDriver *) NULL;
    }
    
/* copy the relevant info from the dbmscap entry into the driver structure */
    db_copy_dbmscap_entry (&driver->dbmscap, cur);
    startup = driver->dbmscap.startup;

/* free the dbmscap list */
    db_free_dbmscap (list);

/* run the driver as a child process and create pipes to its stdin, stdout */

/* open the pipes */
    if ((pipe(p1) < 0 ) || (pipe(p2) < 0 ))
    {
        db_syserror ("can't open any pipes");
	return (dbDriver *) NULL;
    }

/* create a child */
    if ((pid = fork()) < 0)
    {
        db_syserror ("can't create fork");
	return (dbDriver *) NULL;
    }

    if (pid > 0)        /* parent */
    {
        close(p1[READ]);
        close(p2[WRITE]);

/* record driver process id in driver struct */
        driver->pid = pid;

/* convert pipes to FILE* */
	driver->send = fdopen (p1[WRITE], "w");
	driver->recv = fdopen (p2[READ],  "r");

/* most systems will have to use unbuffered io to get the send/recv to work */
#ifndef USE_BUFFERED_IO
	setbuf (driver->send, NULL);
	setbuf (driver->recv, NULL);
#endif

	db__set_protocol_fds (driver->send, driver->recv);
	if(db__recv_return_code(&stat) !=DB_OK || stat != DB_OK)
	    driver =  NULL;
	return driver;
    }
    else        /* child process */
    {
        close(p1[WRITE]);
        close(p2[READ]);

        close (0);
        close (1);

        if (dup(p1[READ]) != 0)
        {
            db_syserror("dup r");
            _exit(127) ;
        }

        if (dup(p2[WRITE]) != 1)
        {
            db_syserror("dup w");
            _exit(127) ;
        }

	execl ("/bin/sh", "sh", "-c", startup, 0);

        db_syserror ("execl");
	return NULL; /* to keep lint, et. al. happy */
    }
}
