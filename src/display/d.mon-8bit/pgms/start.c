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

#include <stdio.h>
#include "monitors.h"

main(argc,argv) char *argv[];
{
	if (argc != 2)
	{
		fprintf(stderr,"Usage:  %s monitor_name\n", argv[0]);
		exit(-1);
	}
	start_mon(argv[1]);
}

start_mon(name)
char *name;
{
	struct MON_CAP *mon;
	struct MON_CAP *R_parse_monitorcap();
	char *ttyname();
	int pid;
	if ((mon = R_parse_monitorcap(MON_NAME,name)) == NULL)
	{
		fprintf(stderr,"Error:  no such monitor '%s'\n",name);
		exit(1);
	}

	if (*mon->tty == '\0' || !strcmp(mon->tty,ttyname(0)))
	{
		if (*mon->tty == '\0')
			execl(mon->path,name,mon->link,(char *) 0);
		else
			execl(mon->path,name,"-",mon->link,(char *) 0);
		fprintf(stderr,"Error:  could not execute '%s'\n",mon->path);
	}
	else
	{
		fprintf(stderr,"Error:  must start %s from %s\n",name,mon->where);
		fprintf(stderr,"You are on %s\n",ttyname(0));
	}
	exit(-1);
}
