#include "globals.h"

char *
whoami()
{
/* NOTE: SQLBASE does NOT accept user names longer than 8 chars */
    static char user[9];

    strncpy (user, db_whoami(), 8);
    user[8] = 0;

    return user;
}
