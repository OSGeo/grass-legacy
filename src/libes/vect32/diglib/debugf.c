#include <stdio.h>
#include <stdarg.h>

/* #include "vask.h" */ /* for the new debug only */
/* WINDOW *INFO_WIN; */

int debugf (char *t,...)
{
	va_list ap;

	va_start(ap,t);
	vfprintf(stderr,t,ap);
	va_end(ap);

	return 0;
}

/* new debugf to get output into v.digit screen - M Neteler 11/2001
 * function taken from v.digit
 */

/*
int _Write_info2 (int line, char *message)
{
    wmove(INFO_WIN,line,1);
    wclrtoeol(INFO_WIN);
    wmove(INFO_WIN,line,1);
    waddstr(INFO_WIN, message);  
    box (INFO_WIN, '|', '-');

    return 0;
}
                        
int debugf (char *message)
{ 
  int line=2;
  
   _Write_info2(line, message); 
   INFO_WIN=newwin (19, 79,  0, 0);
   wrefresh(INFO_WIN);
   
   return 0;
}
*********/
