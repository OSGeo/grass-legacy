#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>

$include sqltypes;

sql_error_code()
{
    return sqlca.sqlcode;
}

sql_error(what)
    char *what;
{
    char err[512];
    char temp[256];
    char temp2[1024];

/* NOTE: rgetmsg(): sometime the messages have %s in them
 *       take advantage of this to insert 'what' into the message
 */
    if (sqlca.sqlcode >= 0)
	return 0;
    rgetmsg ((short)sqlca.sqlcode, temp, sizeof(temp)-1);
    remove_newline(temp);
    if (what && use_sprintf(temp))
	sprintf (temp2, temp, what);
    else
	strcpy (temp2, temp);
    sprintf (err, "Error %d: ", (int)sqlca.sqlcode);
    strcat (err, temp2);
    if (sqlca.sqlerrd[1] < 0)
    {
	sprintf (temp, "\nError %d: ", (int) sqlca.sqlerrd[1]);
	strcat (err, temp);
	rgetmsg ((short)sqlca.sqlerrd[1], temp, sizeof(temp)-1);
	remove_newline(temp);
	strcat (err, temp);
    }

    db_error (err);
    return 1;
}

remove_newline(s)
    char *s;
{
    while (*s && *s != '\n')
	s++;
    *s = 0;
}

static
use_sprintf(s)
    char *s;
{
    int count;

    count = 0;
    for (count = 0; *s; s++)
    {
	if (s[0] == '%')
	{
	    if(s[1] == 's')
		count++;
	    else
		return 0;
	}
    }
    return count==1;
}
