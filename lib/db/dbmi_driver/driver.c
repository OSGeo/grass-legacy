#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include "dbmi.h"
#include "procs.h"
#define	DB_DRIVER_C
#include "dbstubs.h"

extern char *getenv();

/*!
 \fn int db_driver
 \brief 
 \return 
 \param 
*/
int
db_driver(int argc, 
    char *argv[])
{
    int stat;
    int procnum;
    int i;
    int rfd, wfd;
    FILE *send, *recv;

    umask(0);

    /* Read and set enviroment variables, see dbmi_client/start.c */
    if ( getenv ( "GISRC_MODE_MEMORY" ) ) {
        G_set_gisrc_mode ( G_GISRC_MODE_MEMORY );
	G__setenv( "DEBUG", getenv ( "DEBUG" ) );
	G__setenv( "GISDBASE", getenv ( "GISDBASE" ) );
	G__setenv( "LOCATION_NAME", getenv ( "LOCATION_NAME" ) );
	G__setenv( "MAPSET", getenv ( "MAPSET" ) );
	G_debug (3, "Driver GISDBASE set to '%s'", G_getenv ( "GISDBASE" ) );
    }

    send = stdout;
    recv = stdin;

/* THIS CODE IS FOR DEBUGGING WITH CODECENTER */
/**********************************************/
    if (argc == 3)
    {
	rfd = wfd = -1;
	sscanf (argv[1], "%d", &rfd);
	sscanf (argv[2], "%d", &wfd);
	send = fdopen (wfd, "w");
	if (send == NULL)
	{
	    db_syserror(argv[1]);
	    exit(1);
	}
	recv = fdopen (rfd, "r");
	if (recv == NULL)
	{
	    db_syserror(argv[2]);
	    exit(1);
	}
    }
/**********************************************/

    db_clear_error();
    db_auto_print_errors(0);
    db_auto_print_protocol_errors(1);
    db__init_driver_state();

#ifndef USE_BUFFERED_IO
    setbuf (recv, NULL);
    setbuf (send, NULL);
#endif
    db__set_protocol_fds (send, recv);

    if(db_driver_init (argc, argv) == DB_OK)
	db__send_success();
    else
    {
	db__send_failure();
	exit(1);
    }


    stat = DB_OK;
    /* get the procedure number */
    while (db__recv_procnum (&procnum) == DB_OK)
    {
	db_clear_error();

    /* find this procedure */
	for (i = 0; procedure[i].routine; i++)
	    if (procedure[i].procnum == procnum)
		break;

     /* if found, call it */
	if (procedure[i].routine)
	{
	    if((stat = db__send_procedure_ok(procnum)) != DB_OK)
		break; /* while loop */
	    if((stat = (*procedure[i].routine)()) != DB_OK)
		break;
	}
	else if ((stat = db__send_procedure_not_implemented(procnum)) != DB_OK)
	    break;
    }

    db_driver_finish();

    exit (stat == DB_OK ? 0 : 1);
}

