/* %W% %G% */
#include "do_climb.h"
#include "menu.h"
#include <stdio.h>
do_call(command)
	char *command ;
{
	char syscall[256] ;
	char program[256];
	int stat ;
	int (*sigtstp)();
	int (*sigint)();
	char *G_gis_base() ;

	if (sscanf (command, "%s", program) != 1)
		return -1;

/* program has to be allowed by menu */
	if (access(program,0)) 
		return(-1) ;

	sprintf ( syscall, "%s/bin/%s", G_gisbase(), program) ;

	if (access(syscall,1)) 
		return(-1) ;

	sprintf ( syscall, "%s/bin/%s", G_gisbase(), command) ;


	Old_tty();
	screen_clear();
	putchar ('\n');
	stat = execute (syscall) ;
	fprintf(stderr,"Hit RETURN to Continue...") ;
	getchar() ;
	screen_clear() ;
	New_tty();
/*
	touchwin(MenuW) ;
	touchwin(PlanetW) ;
	touchwin(PrmptW) ;
	touchwin(TablW) ;
	touchwin(stdscr) ;
	refresh() ;
*/
	wrefresh (curscr);

	return(stat == 0) ;
}
