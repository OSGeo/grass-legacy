#include <errno.h>

/*
**
**  errors.c
**
**  last modified by David Stigberg 11/14/94
**
*/


static char err_mess [500];
static err_stat = 1;


put_err_mess (err_str, err_no)
   char *err_str;
   int err_no;
{
    strcpy (err_mess, err_str);
	err_stat = err_no;
}

clear_err_mess ()
{
   err_mess [0] = '\0';
   err_stat = 1;
}

get_err_stat ()
{
    return (err_stat);
}

char *
get_err_mess ()
{
	return (err_mess);
}
