#include "stdio.h"
#include "vask.h"
#include <pwd.h>

V__dump_window()

{
	int atrow, atcol ;
	FILE *fopen(), *file ;
	struct passwd *getpwuid() ;
	char home[80] ;
	int curx, cury ;

	sprintf(home,"%s/visual_ask", getpwuid(getuid())->pw_dir ) ;

	if ((file=fopen(home, "a")) == NULL)
	{
		printf("No Copy\n") ;
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



V__remove_trail(ans_col, ANSWER)
	int ans_col ;
	char *ANSWER ;
{
	char *ANS_PTR ;

	ANS_PTR = ANSWER + ans_col ;
	while (ans_col>=0) 
	{
		if ((*ANS_PTR > '\040') && (*ANS_PTR < '\177') && (*ANS_PTR != '\137')) 
			return(0) ;
		*ANS_PTR = 00 ;
		ans_col-- ;
		ANS_PTR-- ;
	}
	return 0;
}
