/* this is like the system() call, except that the current process
   is replaced by the command
*/

#include <stdio.h>
chain (s)
char *s;
{
	fflush (stdout);
	fflush (stderr);

	execl ("/bin/sh", "sh", "-c", s, 0);
	_exit(127);
}
