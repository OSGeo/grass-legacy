#include <stdio.h>
#include "stc123.h"

/*
**  rd_fips123.c
**
** routines in this file are wrappers for calls to fips library read routines
**
** most of them fill the error message buffer on failure via put_err_mess ().
** subsequent calls to get_err_stat () can retrieve the message 
** and error condition
**
** last modified by David Stigberg 11/14/94
*/

open_sdts_rfile (in_name, int_level, ice, ccs, fp)
   char *in_name;
   long *int_level;
   char *ice;
   char *ccs;
   FILE **fp;
{
   char err_mess[200];

   if (!beg123file (in_name, 'r', int_level, ice, ccs, fp))
   {
	   sprintf (err_mess, "Could not open FIPS 123 input file (%s)", in_name);
	   put_err_mess (err_mess, 0);
	   return (0);
   }

   return (1);

}

end_sdts_rfile (filename, fp)
   char *filename;
   FILE **fp;
{
   char err_mess[200];

   if (!end123file (fp))
   {
	  sprintf (err_mess, "Could not close FIPS 123 input file (%s).", filename);
	  put_err_mess (err_mess, 0);
	  return (0);
   }

   return (1);
}


read_dd_rec (filename, fp, str, status)
   char *filename;
   FILE *fp;
   char *str;
   int *status;
{
   char err_mess [200];

   if (!rd123ddrec (fp, str, status ))
   {
	  sprintf (err_mess, "Could not read FIPS 123 Data Descriptive Field for %s.", filename);
	  put_err_mess (err_mess, 0);
	  return (0);
   }

   return (1);

}

read_data_sfld (filename, fp, tag, leadid, str, str_len, status )
   char *filename;
   FILE *fp;
   char *tag;
   char *leadid;
   char *str;
   long *str_len;
   int  *status;
{
	char err_mess[200];

    if (!rd123sfld (fp, tag, leadid, str, str_len, status))
	{
	   sprintf 
		 (err_mess, "Could not read FIPS 123 Data Subfield for %s.", filename);
	   put_err_mess (err_mess, 0);
	   return (0);
    }
	return (1);
}

check_data_sfld (filename, fp, tag, descr, fmt)
   char *filename;
   FILE *fp;
   char *tag;
   char *descr;
   char *fmt;
{
	char err_mess[200];

    if (!chk123sfld (fp, tag, descr, fmt))
	{
	   sprintf 
		 (err_mess, "FIPS 123 chk123sfld failure reading %s.", filename);
	   put_err_mess (err_mess, 0);
	   return (0);
    }
	return (1);
}
