#include "globals.h"

void
report_error (rcd, err)
    SQLTRCD rcd;
    char *err;
{
    SQLTDAL txtlen;
    char msg[4096];
    int len;

    len = 0;
    *msg = 0;
    if (err)
    {
	sprintf (msg, "%s\n\n", err);
	len = strlen(msg);
    }

    sqletx (rcd, SQLXMSG+SQLXREA+SQLXREM, msg+len, sizeof(msg)-1-len, &txtlen);
    db_error (msg);
}

void
report_error_with_carot (cur, rcd, text)
    SQLTCUR cur;
    SQLTRCD rcd;
    char *text;
{
    SQLTEPO epo;	/* error position */
    int n;
    int count, hit;
    char err[4096];
    char tmp[4096];

    sprintf (tmp, "%s\n", text);
    text = tmp;

    sqlepo(cur, &epo);
    *err = 0;
    count = 0;
    hit = 0;
    n = 0;
    do
    {
	if (n == epo)
	    hit = 1;
	err[n++] = *text;
	err[n] = 0;
	if(!hit)
	    count++;

	if (*text == '\n')
	{
	    if (hit == 1)
	    {
		n = add_carot (err, n, count);
		hit = 2;
	    }
	    count = 0;
	}
    } while (*text++);
    if (hit!=2)
	add_carot (err, epo, count);

    report_error (rcd,  err);
}

static
add_carot (err, n, count)
    char *err;
    int n;
{
    if (err[n-1] != '\n')
	err[n++] = '\n';
    while (count-- > 0)
	err[n++] = ' ';
    err[n++] = '^';
    err[n++] = '\n';
    err[n] = 0;

    return n;
}
