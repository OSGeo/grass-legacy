/*
 * Search Type 7 records for Points with required CFCC's. Write each to
 * GRASS site_list or vector file.
 */
#include "gis.h"
#include "site.h"
#include "Vect.h"
#include "globals.h"

int do_points (void)
{
  int n, both_kinds;
  char buff[100];

  if (feature_type[1][0] != 'P')
    return 0;			/* don't do anything */
  open_site_file ();
  tig_open ('7');
  n = 0;
  both_kinds = both_conds ();
  is_a_match ("~!~start!", "");
  while (get_tiger_record (tiger[7].fp, '7', buff + 1) != EOF)
    if (point_yes (buff) && good_one ('P', buff, both_kinds))
    {
      save_site (buff);
      n++;
    }
  tig_close ('7');
  close_site_file (n);
  return n;
}

/* check for a point record (missing lat-long is not a point) */
int point_yes (char *buf)
{
  return ((buf[59] == ' ') ? 0 : 1);
}

int open_site_file (void)		/* open site list */
{
  char dat[20];
  FILE *fp1;
  Site_head info;

  if (sitefile == NULL)
  {
    if ((sitefile = G_fopen_sites_new (site_name)) == NULL)
      G_fatal_error ("Could not open requested site list.");
    else
    {
      fp1 = popen ("date +%m/%d/%y", "r");	/* get the current date */
      fscanf (fp1, "%10s", dat);
      pclose (fp1);
      info.desc=(char *)G_malloc(80*sizeof(char));
      sprintf (info.desc, "Census Landmark points extracted %s", dat);
      info.name=G_strdup(site_name);
      info.labels=(char *)G_malloc(80*sizeof(char));
      sprintf (info.labels, 
               "easting,northing,#landmark_number,@CFCC,@\"Feature name\"");
      G_site_put_head(sitefile,&info);
    }
    return 1;
  }
  return 0;
}

int save_site (char *buf)
{
  Site *s;

  s=G_site_new_struct(CELL_TYPE, 2,2,0);

  get_point_coords (buf + 55, &(s->east), &(s->north));

  if (sitefile)
  {				/* comment = #landmark# CFCC Feature name */
    sscanf(buf+11,"%d", &(s->ccat));
    sscanf (buf+22, "%3s", s->str_att[0]);
    G_strncpy (s->str_att[1],buf+25,30);
    G_squeeze (s->str_att[1]);	/* remove trailing blanks */
    G_site_put (sitefile, s);
  }
  else
    G_fatal_error ("No site file open to write to.");
}

int close_site_file (int n)
{
  if (sitefile != NULL)
  {
    fclose (sitefile);
    if (!n)
      G_remove ("site_lists", site_name);
    sitefile = NULL;
  }

  return 0;
}
