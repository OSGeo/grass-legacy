/*
**
** control.c
**
**  last modified by David Stigberg 11/94
**
*/

#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "defines.h"
#include "externs.h"
#include "globals2.h"

#define IDEN_COMT_TYPE 1
#define DDDF_GRASS_ENT_TYPE 2
#define DDDF_ATTR_NUM_TYPE  3
#define DDDF_ATTR_LABEL_TYPE 4

char * get_cats_maxlen ();

get_metadata (map, meta_flag, dig_name, mapset)
   struct Map_info *map;
   int meta_flag;
   char *dig_name, *mapset;
{

   FILE *fpin, *fopen();
   char errmsg[200];
   char meta_filename[200];

   sprintf (meta_filename, "%s/%s/dig_misc/%s/sdts_meta/metafile",
	  G_location_path(), G_mapset(), dig_name);

   *Iden_titl = *Iden_mpdt = *Xref_hdat = *Iden_comt = '\0';

   /*get metadata from user's metafile, if it exists*/
   if (meta_flag)
   {
   if (access (meta_filename, 0) == 0)
   {
	   if ((fpin =  fopen (meta_filename, "r")) == NULL) {
		   sprintf (errmsg, "Unable to read metafile:\n <%s>\n", meta_filename);
		   G_fatal_error (errmsg);
	   }
       read_metadata_from_user_metafile (fpin);
	   fclose (fpin);
   }
   else
   {
	   sprintf (errmsg, "Unable to access metafile:\n <%s>\n", meta_filename);
	   G_fatal_error (errmsg);
   }
   }

 /*if these were not defined in user's metafile, copy defaults*/

   if (! (*Iden_titl)) 
	   strcpy (Iden_titl, map->head.map_name);
   if (! (*Iden_comt)) 
	   strcpy (Iden_comt, IDEN_COMT);
   /* revision: since HDAT is optional field, only use if supplied by user */
   /*
   if (! (*Xref_hdat))
	   strcpy (Xref_hdat, "NAS");
	   */

   get_generated_dd_entries (map, dig_name, mapset);

}

read_metadata_from_user_metafile (fpin)
	FILE *fpin;
{
    char buf[BUFSIZ];
	char multibuf[5000], tmp_multibuf[200];
	int len, type = 0, tmp_type = 0, prev_is_multi = 0,  process_multi = 0;

	multibuf[0] = tmp_multibuf[0] = '\0';

	while ((fgets (buf, 250, fpin)) != NULL)
	{
		/*strip newline*/
		G_squeeze (buf);
		len = strlen (buf);
		buf [len] = '\0';


	    if (process_multi)
		{
	       process_multi_line_metadata_entry (multibuf, type);
		   strcpy (multibuf, tmp_multibuf);
		   tmp_multibuf[0] = '\0';
		   type = tmp_type;
		}
	    process_multi = 0;

		if (strncmp (buf, "IDEN_MPDT:", 10) == 0)
		{

			 if (strlen (buf) < 11)
				 continue;
			 if (prev_is_multi)
				process_multi = 1;
             prev_is_multi = 0;

			 G_strncpy (Iden_mpdt, buf +  strlen ("IDEN_MPDT:"),  8);
			 len = strlen (Iden_mpdt);
			 if (len != 4 && len != 8)
			 G_fatal_error ("Metadata file: Map date error\n");
			 Iden_mpdt[len] = '\0';
        }
        else
		if (strncmp (buf, "IDEN_TITL:", 10)== 0)
		{
			 if (strlen (buf) < 11)
				 continue;
			 if (prev_is_multi)
				process_multi = 1;
             prev_is_multi = 0;

			 G_strncpy (Iden_titl, buf +  strlen ("IDEN_TITL:"), 50);
			 len = strlen (Iden_titl);
			 Iden_titl[len] = '\0';
        }
        else
		if (strncmp (buf, "IDEN_COMT:", 10)== 0)
		{
			 if (strlen (buf) < 11)
				 continue;
			 if (prev_is_multi)
			 {
				process_multi = 1;
				strcpy (tmp_multibuf, buf);
			 }
			 else
				strcpy (multibuf, buf);
             prev_is_multi = 1;
			 type = IDEN_COMT_TYPE;
        }
        else
		if (strncmp (buf, "XREF_HDAT:",  10)== 0)
		{
			 if (strlen (buf) < 11)
				 continue;
			 if (prev_is_multi)
				process_multi = 1;
             prev_is_multi = 0;

			 G_strncpy (Xref_hdat, buf +  strlen ("XREF_HDAT:"), 4);
			 Xref_hdat[3] = '\0';
        }
		else
		if (strncmp (buf, "DDDF_GRASS_ENT:", 15)== 0)
		{
			 if (strlen (buf) < 16)
				 continue;
			 if (prev_is_multi)
			 {
				process_multi = 1;
				strcpy (tmp_multibuf, buf);
			    tmp_type = DDDF_GRASS_ENT_TYPE;
			 }
			 else
			 {
				strcpy (multibuf, buf);
			    type = DDDF_GRASS_ENT_TYPE;
			 }
             prev_is_multi = 1;

		}
		else
		if (strncmp (buf, "DDDF_ATTR_NUM:", 14)== 0)
		{
			 if (strlen (buf) < 15)
				 continue;
			 if (prev_is_multi)
			 {
				process_multi = 1;
				strcpy (tmp_multibuf, buf);
			    tmp_type = DDDF_ATTR_NUM_TYPE;
			 }
			 else
			 {
				strcpy (multibuf, buf);
			    type = DDDF_ATTR_NUM_TYPE;
			 }
             prev_is_multi = 1;

		}
		else
		if (strncmp (buf, "DDDF_ATTR_LABEL:", 16)== 0)
		{
			 if (strlen (buf) < 17)
				 continue;
			 if (prev_is_multi)
			 {
				process_multi = 1;
				strcpy (tmp_multibuf, buf);
			    tmp_type = DDDF_ATTR_LABEL_TYPE;
			 }
			 else
			 {
				strcpy (multibuf, buf);
			    type = DDDF_ATTR_LABEL_TYPE;
			 }
             prev_is_multi = 1;

		}
		else
		if (strncmp (buf, "DDSH_ENT_NAME:", 14)== 0)
		{
			 if (strlen (buf) < 15)
				 continue;
			 if (prev_is_multi)
				process_multi = 1;
             prev_is_multi = 0;

			 get_grass_ddsh_entry ("DDSH_ENT_NAME:", "ATTR_NUM", buf);
			 get_grass_ddsh_entry ("DDSH_ENT_NAME:", "ATTR_LABEL", buf);
		}
		else
		/*we have a line without a tag*/
		{
			if (prev_is_multi)
			{
			   /*add line to bigbuf*/
			   strcat (multibuf, "  "); /*replace newlines with spaces*/
			   strcat (multibuf, buf);
			}
			else
			 /*error-illegal line; skip*/
			 {
			 G_warning ("Metadata file with illegal line\n");
			 }
		}
	}

   /*write out last multi-line record, if any*/
	    if (process_multi)
	       process_multi_line_metadata_entry (multibuf, type);
}

#include <time.h>

char *malloc();

char *
get_IDEN_DCDT ()
{
  long clock;
  struct tm *local;
  char *date;

  struct tm *localtime();

  date = malloc ( sizeof (char) * 10 );

  time (&clock);
  local = localtime (&clock);

  sprintf (date, "%4d%02d%02d",local->tm_year + 1900, local->tm_mon + 1, local->tm_mday);

  return (date);
}  

char *
get_IDEN_DAST (map)
  struct Map_info *map;
{
   char dast_str[200];

   sprintf (dast_str, "GRASS %d.%d vector format-transfer includes entity points and area points with pointers to enclosing GT-polygons", map->head.Version_Major, map->head.Version_Minor);

   return (G_store(dast_str));
}

get_grass_dddf_entry (tag, label, buf)
  char *tag, *label, *buf;
{
	int i, len;
	char *p;

	 len = strlen (buf) - strlen (tag);
	 p = G_malloc (len + 1);
	 G_strncpy (p, buf +  strlen (tag), len);
	 p[len] = '\0';


     for (i = 1; Dddf[i].rcid != 0; i++)
       if (strcmp (Dddf[i].ealb, label) == 0)
	   {
		   Dddf[i].dfin = p; 
           return;
       }
}

get_grass_ddsh_entry (tag, label, buf)
  char *tag, *label, *buf;
{
	int i, len;
	char *p;

	 len = strlen (buf) - strlen (tag);
	 p = G_malloc (len + 1);
	 G_strncpy (p, buf +  strlen (tag), len);
	 p[len] = '\0';


     for (i = 1; Ddsh[i].rcid != 0; i++)
       if (strcmp (Ddsh[i].atlb, label) == 0)
	   {
		   Ddsh[i].etlb = p; 
           return;
       }
}

get_generated_dd_entries (map, dig_name, mapset)
   struct Map_info *map;
   char *dig_name, *mapset;
{
   P_ATT *Att;
   register int i;
   int first_time = 1;
   int min = 0;
   int max = 0;
   int maxlen;
   char *pmin, *pmax, *pmaxlen, *pline_3_len;
   char *x_etlb_for_attrib;


   for (i = 1; i <= map->n_atts; i++)
   {
	   Att = &(map->Att[i]);
	   if (first_time)
	   {
		  min = max = Att->cat;
		  first_time = 0;
	   }
       else
	   {
		 min = min > Att->cat ? Att->cat : min;
		 max = max < Att->cat ? Att->cat : max;
	   }
   }

   /*ddom: ATTR_NUM MIN and MAX*/
   pmin = G_malloc (14);
   sprintf (pmin, "%d", min);
   pmax = G_malloc (14);
   sprintf (pmax, "%d", max);

   /*ddsh: ATTR_NUM MXLN*/
   maxlen = strlen (pmax);
   pmaxlen = G_malloc (4);
   sprintf (pmaxlen, "%d", maxlen);

   /*ddsh: OTHER_INFO MXLN*/
   pline_3_len = G_malloc (3);;
   if (map->head.Version_Major > 3)
	   maxlen = NEW_LINE_3_SIZE - 1;
   else
	   maxlen = OLD_LINE_3_SIZE - 1;
   sprintf (pline_3_len, "%d", maxlen);

  /*ddsh: default entity label = dig_name */
   maxlen = strlen (dig_name);
   x_etlb_for_attrib = G_malloc (maxlen + 1);
   strcpy (x_etlb_for_attrib, dig_name);
    
   for (i = 1; Ddom[i].rcid != 0; i++)
       if (strcmp (Ddom[i].atlb, "ATTR_NUM") == 0)
	   {
		   if (strcmp (Ddom[i].rava, "MIN") == 0)
		       Ddom[i].dval = pmin; 
		   else if (strcmp (Ddom[i].rava, "MAX") == 0)
		       Ddom[i].dval = pmax; 
       }

   for (i = 1; Ddsh[i].rcid != 0; i++)
   {
       if (strcmp (Ddsh[i].atlb, "ATTR_NUM") == 0)
	   {
		   Ddsh[i].mxln = pmaxlen; 
		   /*if etlb hasn't already been supplied from metafile*/
		   if (! *Ddsh[i].etlb)
			   Ddsh[i].etlb = x_etlb_for_attrib; 
	   }
       if (strcmp (Ddsh[i].atlb, "OTHER_INFO") == 0)
		   Ddsh[i].mxln = pline_3_len; 
       if (strcmp (Ddsh[i].atlb, "ATTR_LABEL") == 0)
	   {
		   Ddsh[i].mxln = get_cats_maxlen (map, dig_name, mapset);
		   /*if etlb hasn't already been supplied from metafile*/
		   if (! *Ddsh[i].etlb )
			   Ddsh[i].etlb = x_etlb_for_attrib; 
	   }
   }
}

char *
get_cats_maxlen (map, dig_name, mapset)
   struct Map_info *map;
   char *dig_name, *mapset;
{
    struct Categories cats;
    P_ATT *Att;
	int cat_stat, i, maxlen, tmplen;
	char *pmaxlen;

	pmaxlen = G_malloc (5);

	cat_stat = get_cats_struct (dig_name, mapset, &cats);

	if (cat_stat < 0)
	{
	  strcpy (pmaxlen, "");
	  return (pmaxlen); 
    }

	
   maxlen = 0;

   for (i = 1; i <= map->n_atts; i++)
   {
	   Att = &(map->Att[i]);
	   tmplen  =  strlen (G_get_cat (Att->cat, &cats));
	   maxlen = (tmplen > maxlen) ? tmplen : maxlen;
   }
   sprintf (pmaxlen, "%d", maxlen);
   return (pmaxlen);
}


process_multi_line_metadata_entry (buf, type)
  char *buf;
  int type;
{

   switch (type)
   {
	  case IDEN_COMT_TYPE:
		 G_strncpy (Iden_comt, buf  +  strlen ("IDEN_COMT:") , 2000 );
		 break;
	  case DDDF_GRASS_ENT_TYPE:
		get_grass_dddf_entry ("DDDF_GRASS_ENT:", "GRASS_ENT", buf);
		break;
	  case DDDF_ATTR_NUM_TYPE:
	    get_grass_dddf_entry ("DDDF_ATTR_NUM:", "ATTR_NUM", buf);
		break;
	  case DDDF_ATTR_LABEL_TYPE:
	    get_grass_dddf_entry ("DDDF_ATTR_LABEL:", "ATTR_LABEL", buf);
		break;
      default:
		break;
   }
}
