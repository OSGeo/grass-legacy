static char rcsid[] = "@(#)XGRASS $Id: user.c,v 0.0 1992/05/05 14:56:26 sink Exp sink $";
/*
* File: user.c
*
* Desc: contains code for getting user information
*
* Auth: Eric W. Sink
*
* Date: Tue Nov  5 16:21:47 CST 1991
*
* Modification History:
*
*
*/

#include "xgrass_lib.h"
#include <pwd.h>
 
#ifdef _NO_PROTO
char *
_XgGetUserName()
#else
char *
_XgGetUserName(void)
#endif
{
    struct passwd *data;
    data = getpwuid(getuid());
    return data->pw_name;
}

