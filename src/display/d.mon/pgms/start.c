/* 
 * $Id$
 * 
 * Changed for truecolor 24bit support by 
 * Roberto Flor/ITC-Irst, Trento, Italy
 * August 1999
 *
*/
     


/****************************************************************
 * start_mon - start a monitor running
 *
 * read $GISBASE/etc/monitorcap
 *
 * If the monitor can be started from any tty, it will be run in
 * background.  If not, i.e., if there is an entry in the monitorcap
 * file which forces the monitor to be started from a specific tty,
 * then the monitor will run in foreground.  To run the monitor in
 * background, we invoke it with only the fifo names as an argument.
 * To run it in foreground, "-" is the first argument and the fifo
 * names are the second.  When the monitor runs in background, the
 * fork is actually done in the monitor itself.  This prevents the
 * monitor startup message from appearing after the user gets his
 * prompt back.  For more information, see the comments in SWITCHER.c.
 ****************************************************************/

#include "config.h"
#include <unistd.h>
#include <stdio.h>
#include "raster.h"
#include "monitors.h"
#include "local_proto.h"

#ifdef __W98__

#include <process.h>

#define execl(fullpath,path,name,bg,link,par,nul)		\
do {								\
	spawnl(_P_DETACH,fullpath,path,name,"-",link,par,nul);	\
	return 0;						\
} while (0)

#endif /* __W98__ */

int main (int argc, char *argv[])
{
	if (argc < 2 || argc > 3)
	{
		fprintf(stderr,"Usage:  %s monitor_name [par]\n", argv[0]);
		return 1;
	}

	return start_mon(argv[1], (argc == 3) ? argv[2] : "");
}

int start_mon (char *name, char *par)
{
	struct MON_CAP *mon;
	int pid;

	if ((mon = R_parse_monitorcap(MON_NAME,name)) == NULL)
	{
		fprintf(stderr,"Error:  no such monitor '%s'\n",name);
		exit(1);
	}

	if (*mon->tty != '\0' && strcmp(mon->tty,ttyname(0)) != 0)
	{
		fprintf(stderr,"Error:  must start %s from %s\n",name,mon->where);
		fprintf(stderr,"You are on %s\n",ttyname(0));
		exit(1);
	}

	execl(	mon->path,
		mon->path,
		name,
		*mon->tty == '\0' ? "" : "-",
		mon->link,
		par,
		(char *) 0);

	perror("Could not execute monitor");
	return 1;
}
