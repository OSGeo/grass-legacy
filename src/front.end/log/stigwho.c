/*
 * whoisuser - identify user by name
 *
 * returns a pointer to a 'safe' (non-static) string identifying
 * the user, obtained from getlogin(), getpwuid(), or whatever.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include "defines.h"

char *
whoisuser ()
{
    char *name;
    char *ename;
    char *lname;
    char *sname = ADMINISTRATOR;
    struct passwd *pw;

    if (! (lname = getlogin ()) || !*lname)	/* Get the login name	*/
      if (! (pw = getpwuid (getuid ())))
	lname = "login name unknown?";
      else
	lname = pw->pw_name;

    if ((ename = getenv ("USER")))	/* Get environment name	*/
      if (strcmp(sname,lname))
	name = lname;
      else
	name = ename;
    else name = lname;

    return (strcpy (malloc (strlen (name) + 1), name));
}
