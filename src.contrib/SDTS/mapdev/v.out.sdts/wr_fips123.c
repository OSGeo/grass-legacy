#include <stdio.h>
#include "stc123.h"

/*
**  wr_fips123.c
**
** routines in this file are wrappers for calls to fips library write routines
**
** most of them fill the error message buffer on failure via put_err_mess ().
** subsequent calls to get_err_stat () can retrieve the message 
** and error condition
**
** last modified by David Stigberg 11/14/94
*/

Open_sdtsfile (sdts_path, file_name, fp, mode)
   char *sdts_path;
   char *file_name;
   FILE **fp;
   char mode;
{
   char full_name[250];

   sprintf (full_name, "%s/%s", sdts_path, file_name);

   return (open_sdtsfile (full_name, fp, mode));
}

open_sdtsfile (out_name, fp, mode)
   char *out_name;
   FILE **fp;
   char mode;
{
   long int_level;
   char ccs[4];
   char ice[2];
   char err_mess[200];

   int_level = 2;
   strcpy (ice," ");
   strcpy (ccs,"   ");

   if (!beg123file (out_name, mode, &int_level, ice, ccs, fp))
   {
	   sprintf (err_mess, "Could not open FIPS 123 output file (%s)", out_name);
	   put_err_mess (err_mess, 0);
	   return (0);
   }

   return (1);

}

end_sdtsfile (filename, fp)
   char *filename;
   FILE **fp;
{
   char err_mess[200];

   if (!end123file (fp))
   {
	  sprintf (err_mess, "Could not close FIPS 123 output file (%s).", filename);
	  put_err_mess (err_mess, 0);
	  return (0);
   }

   return (1);
}

#define TAG_ZERO    "0000"
#define CONTROL_ZERO "0000;&"
#define TAG_ONE     "0001"
#define CONTROL_ONE "0100;&"


begin_sdts_ddr (filename, fpout)
   char *filename;
   FILE *fpout;
{
	char cstrng [5000];
	char tag [10];
	int option;
	char err_mess[200];


    if (!beg123ddrec (fpout))
	{
	   sprintf (err_mess, "Could not begin FIPS 123 Data Descriptive Record for %s.", filename);
	   put_err_mess (err_mess, 0);
	   return (0);
    }

	option = 2; /*start of record*/

	strcpy (cstrng, CONTROL_ZERO);
	strcat (cstrng, filename);
	strcat (cstrng, FT_STR);

	if (!wr123ddfld (fpout, TAG_ZERO, cstrng, option))
	{
	   sprintf (err_mess, "Could not write FIPS 123 Data Descriptive Field for %s.", filename);
	   put_err_mess (err_mess, 0);
	   return (0);
    }

	strcpy (cstrng, CONTROL_ONE);
	strcat (cstrng, "DDF RECORD IDENTIFIER");
	strcat (cstrng, FT_STR);

	option = 6; /*start of field but not start of rec*/

	if (!wr123ddfld (fpout, TAG_ONE, cstrng, option))
	{
	   sprintf (err_mess, "Could not write FIPS 123 Data Descriptive Field for %s.", filename);
	   put_err_mess (err_mess, 0);
	   return (0);
    }

	return (1);

}

begin_data_rec (filename, fpout)
  char *filename;
  FILE *fpout;
{
	 char err_mess[200];

     if (!beg123rec (fpout))
	 {
		 sprintf (err_mess, "Could not begin FIPS 123 Data Record for %s.", filename);
		 put_err_mess (err_mess, 0);
		 return (0);
	 }

	 return (1);
}

end_data_rec (filename, fpout)
  char *filename;
  FILE *fpout;
{
	 char err_mess[200];

     if (!end123rec (fpout))
	 {
		 sprintf (err_mess, "Could not end FIPS 123 Data Record for %s.", filename);
		 put_err_mess (err_mess, 0);
		 return (0);
	 }

	 return (1);
}

end_dd_rec (filename, fpout)
  char *filename;
  FILE *fpout;
{
	 char err_mess[200];

     if (!end123ddrec (fpout))
	 {
		 sprintf (err_mess, "Could not end FIPS 123 Data Descriptive Record for %s.", filename);
		 put_err_mess (err_mess, 0);
		 return (0);
	 }

	 return (1);
}

build_ddr_fldstr (ddr_str, name_str, label_str, fmt_str)
   char *ddr_str, *name_str, *label_str, *fmt_str;
{
   *ddr_str = NC;

   strcpy (ddr_str, name_str);
   strcat (ddr_str, UT_STR);
   strcat (ddr_str, label_str);
   strcat (ddr_str, UT_STR);
   strcat (ddr_str, fmt_str);
   strcat (ddr_str, FT_STR);
   return;
}

write_dd_fld (filename, fpout, tag, name_str, label_str, fmt_str, option)
   char *filename;
   FILE *fpout;
   char *tag, *name_str, *label_str, *fmt_str;
   int option;
{
   char err_mess [200];
   char ddr_str [500];

   *ddr_str = NC;
   strcpy (ddr_str, name_str);
   strcat (ddr_str, UT_STR);
   strcat (ddr_str, label_str);
   strcat (ddr_str, UT_STR);
   strcat (ddr_str, fmt_str);
   strcat (ddr_str, FT_STR);

   if (!wr123ddfld (fpout, tag, ddr_str, option))
   {
	  sprintf (err_mess, "Could not write FIPS 123 Data Descriptive Field for %s.", filename);
	  put_err_mess (err_mess, 0);
	  return (0);
   }

   return (1);

}

write_data_fld (filename, fpout, tag, leadid, wr_str, str_len, option )
   char *filename;
   FILE *fpout;
   char *tag;
   char leadid;
   char *wr_str;
   long str_len;
   int option;
{
	char err_mess[200];

    if (!wr123fld (fpout, tag, leadid, wr_str, str_len, option))
	{
	   sprintf 
		 (err_mess, "Could not write FIPS 123 Data Field for %s.", filename);
	   put_err_mess (err_mess, 0);
	   return (0);
    }
	return (1);
}
