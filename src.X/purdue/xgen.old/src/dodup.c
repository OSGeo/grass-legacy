#include "xgen.h"

void DoDup(s,des)
	char *s;
	int des;
{
	int tty;

	if ((tty = open(s,O_RDWR|O_CREAT,S_IREAD|S_IWRITE)) < 0) {
    	perror(s);
    	_exit(1);
    }
    if ( tty != 0 ) (void)dup2(tty,des);
}
