/***************************************************************
 *  XgenExit - exit after wiping out any remaining processes
 *             spawned by Xgen.
 *
 *	input 		- an integer status returned on exit()
 *  returns 	- ...never...
 **************************************************************/
#include "xgen.h"

void
XgenExit(status)
	int status;
{
	Command *com;
	
	/***************************************************************
	 * kill all pending processes...
	 **************************************************************/
	 for ( com = xgenGD.commandList; com != NULL; com = com->next ) {
		int pgrp;
		char buf[80];

		/* unlink temp files if they exist */
		if ( com->capture && !access(com->tmpfile,0) )
			unlink(com->tmpfile);
		if ( !access(com->errfile,0) )
			unlink(com->errfile);
		/* check for existance */
		if ( kill(com->pid,0) == 0 && errno != ESRCH ) {
			/* get the process group and wipe it out */
			pgrp = getpgrp(com->pid);
			if ( killpg(pgrp,SIGINT) < 0 ) {
				sprintf(buf,"killpg: process group %d",pgrp);
				perror(buf);
			}
	 	}
	 }
	/***************************************************************
	 * and quit....
	 **************************************************************/
	 exit(status);
}
