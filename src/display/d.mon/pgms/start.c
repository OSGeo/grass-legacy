
/* Changed for truecolor 24bit support by 
 * Roberto Flor/ITC-Irst, Trento, Italy
 * August 1999
 *
 * added new parameter "nlev" to specify number of colors per color channel
 * example; nlev=8 means 8bit for each R, G, B equal to 24bit truecolor
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


int main (int argc, char *argv[])
{
#ifdef ORIG
	if (argc != 2)
	{
		fprintf(stderr,"Usage:  %s monitor_name\n", argv[0]);
		exit(-1);
	}
#else /* ORIG */
	if (argc < 2)
	{
		fprintf(stderr,"Usage:  %s monitor_name [par]\n", argv[0]);
		exit(-1);
	}
#endif /* ORIG */
#ifdef ORIG
	start_mon(argv[1]);
#else /* ORIG */
	if ( argc == 3 ) 
	 start_mon(argv[1],argv[2]);
	else
	 start_mon(argv[1],"");
#endif /* ORIG */
	return 0;
}
#ifdef ORIG
int start_mon (char *name)
#else /* ORIG */
int start_mon (char *name, char *par)
#endif /* ORIG */
{
	struct MON_CAP *mon;
	int pid;
	if ((mon = R_parse_monitorcap(MON_NAME,name)) == NULL)
	{
		fprintf(stderr,"Error:  no such monitor '%s'\n",name);
		exit(1);
	}

	if (*mon->tty == '\0' || !strcmp(mon->tty,ttyname(0)))
	{
		if (*mon->tty == '\0')
#ifdef ORIG
			execl(mon->path,name,mon->link,(char *) 0);
#else /* ORIG */
		{
#ifdef USE_G_SOCKS
		if ( par[0] == '\0')
		    execl(mon->path,name,(char *) 0);
		else
	            execl(mon->path,name,par,(char *) 0);
#else
		if ( par[0] == '\0')
			execl(mon->path,name,mon->link,(char *) 0);
		else
			 execl(mon->path,name,mon->link,par,(char *) 0);
#endif
		}
#endif /* ORIG */
		else
#ifdef ORIG
			execl(mon->path,name,"-",mon->link,(char *) 0);
#else /* ORIG */
		{
			if ( par[0] == '\0')
			execl(mon->path,name,"-",mon->link,(char *) 0);
			else 
			 execl(mon->path,name,"-",mon->link,par,(char *) 0);
		}
#endif /* ORIG */
		fprintf(stderr,"Error:  could not execute '%s'\n",mon->path);
	}
	else
	{
		fprintf(stderr,"Error:  must start %s from %s\n",name,mon->where);
		fprintf(stderr,"You are on %s\n",ttyname(0));
	}
	exit(-1);
}
