#include "gis.h"

int main (argc, argv)
  char **argv;
  int argc;
{
  char *mapset, *sitefile, *tmpfile = NULL, errmsg[200];
  char sbuf[MAX_SITE_LEN], *buf;
  FILE *fdsite, *tmp;
  struct
  {
    struct Option *input;
  } parm;

  G_gisinit (argv[0]);

  parm.input = G_define_option ();
  parm.input->key = "sites";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "name of a sites file to be converted";
  parm.input->gisprompt = "old,site_lists,sites,input";

  if (G_parser (argc, argv))
    exit (1);

  sitefile = parm.input->answer;

  if ((mapset = G_find_file ("site_lists", sitefile, G_mapset ())) == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found in current mapset", sitefile);
    G_fatal_error (errmsg);
  }

  if ((fdsite = G_fopen_sites_old (sitefile, mapset)) == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", sitefile);
    G_fatal_error (errmsg);
  }
  tmpfile = G_tempfile ();
  if ((tmp = fopen (tmpfile, "w")) == NULL)
  {
    sprintf (errmsg, "can't open temporary file [%s]", tmpfile);
    G_fatal_error (errmsg);
  }

  while ((buf = fgets (sbuf, 1024, fdsite)) != (char *) NULL)
  {
    if (*buf == '#' || !isdigit (*buf))
      fprintf (tmp, "%s", buf);
    else
    {
      while (*buf)
      {
	if (*buf == '#')
	  *buf = '%';
	fprintf (tmp, "%c", *buf);
	buf++;
      }
    }
  }
  fclose (tmp);
  fclose (fdsite);

  /* reopen site for writing, tmp for reading */
  if ((fdsite = G_fopen_sites_new (sitefile)) == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s] for writing", sitefile);
    G_fatal_error (errmsg);
  }
  if ((tmp = fopen (tmpfile, "r")) == NULL)
  {
    sprintf (errmsg, "can't reopen temporary file [%s]", tmpfile);
    G_fatal_error (errmsg);
  }
  /* copy files */

  if (cp_filep (tmp, fdsite))
    G_fatal_error ("error rewriting sites file");
  unlink (tmpfile);
  exit (0);
}

int cp_filep (in, out)
  FILE *in, *out;
{
  char buf[BUFSIZ];
  int red, ret;
  int no_file = 0;
  int err = 0;

  fseek (in, 0L, 0);
  {
    while (red = fread (buf, 1, BUFSIZ, in))
    {
      if (!(ret = fwrite (buf, 1, red, out)))
      {
	err++;
	break;
      }
    }
    fclose (in);
  }
  if (0 != fclose (out))
    err++;

  return (err);
}
