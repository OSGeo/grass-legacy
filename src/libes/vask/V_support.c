#include <stdio.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include <curses.h>
#include "config.h"
#include "vask.h"

int V__dump_window()
{
	int atrow, atcol ;
	FILE *file ;
	char home[80] ;
	int curx, cury ;

	sprintf(home,"%s/visual_ask", getpwuid(getuid())->pw_dir ) ;

	if ((file=fopen(home, "a")) == NULL)
	{
		fprintf (stdout,"No Copy\n") ;
		return(-1) ;
	}

	getyx(stdscr, cury, curx) ;

	fprintf(file,"--------------------------------------------------------\n") ;
	for (atrow=0; atrow<LINES; atrow++)
	{
		for (atcol=0; atcol<COLS-1; atcol++)
		{
			move(atrow, atcol) ;
			fprintf(file,"%c",inch()) ;
		}
		fprintf(file,"\n") ;
	}
	fprintf(file,"--------------------------------------------------------\n") ;
	fprintf(file,"\n\n") ;
	fclose(file) ;

	move(cury, curx) ;
	return 0;
}


int V__remove_trail( int ans_col , char *ANSWER )
{
	char *ANS_PTR ;

	ANS_PTR = ANSWER + ans_col ;
	while (ans_col>=0) 
	{
		int c = *(unsigned char *)ANS_PTR;
		if (c > '\040' && c != '\177' && c != '_')
			return 0 ;
		*ANS_PTR = '\0' ;
		ans_col-- ;
		ANS_PTR-- ;
	}
	return 0;
}
