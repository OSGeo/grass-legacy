/****************************************************************
this program runs its arguments as  a  commmand.  it  essentially
does what the sh would do to look for the command. if / occurs in
the command it runs it  as  is,  otherwise  it  search  the  PATH
variable.  care  is  taken  to preserve the PATH variable that is
passed (as part of the environment) to the command being invoked.

the signals SIGINT and SIGQUIT are  set  to  the  default  action
before running the command.

This  program  is  needed  because  the  GIS  shell  must  ignore
interrupts when it runs the user's shell. There is no way to tell
the user's shell to re-activate interrupts in shell-ese.
****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include "local_proto.h"

int main (int argc, char *argv[])
{
    char *PATH;
    char path[1024];
    char *a,*b,*c;
    int more;

    signal (SIGINT, SIG_DFL);
    signal (SIGQUIT, SIG_DFL);

    argc--;
    argv++;
    if (argc <= 0) exit(1);

/* if argv[0] contains a /, run directly */
    for (a = argv[0]; *a; a++)
	if (*a == '/')
	{
	    execv (argv[0],argv);
	    fprintf (stderr, "%s: Command not found\n",argv[0]);
	    exit(127);
	}

/* else search PATH */
    a = PATH = getenv("PATH");
    more = 1;
    while (more)
    {
	for (b = a; *b && (*b != ':'); b++)
		;
	if (*b == 0) more=0;
	if (*a)
	{
	    c = path;
	    while (a != b)
		*c++ = *a++;
	    *c++ = '/';
	    a = argv[0];
	    while (*a)
		*c++ = *a++;
	    *c = 0;
	    execv (path, argv);
	}
	a = b + 1;
    }
    fprintf (stderr, "%s: Command not found\n",argv[0]);

    exit(0);
}
