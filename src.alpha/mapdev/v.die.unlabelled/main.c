#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "version.h"

main (argc, argv)
  char *argv[];
{
  struct Map_info oMap,nMap;
  struct Option *oldvect, *newvect;
  struct Flag *Nosup;
  char *mapset;
  char errmsg[1000];
  int level;
  struct line_pnts *Points;
  FILE *ovect, *nvect;
  double X, Y;
  int i, ret;
  int cnt = 0;
  int label;

  G_gisinit (argv[0]);

  oldvect = G_define_option ();
  oldvect->key = "input";
  oldvect->type = TYPE_STRING;
  oldvect->required = YES;
  oldvect->multiple = NO;
  oldvect->gisprompt = "old,dig,vector,input";
  oldvect->description = "input vector file";

  newvect = G_define_option ();
  newvect->key = "output";
  newvect->type = TYPE_STRING;
  newvect->required = YES;
  newvect->multiple = NO;
  newvect->gisprompt = "new,dig,vector,output";
  newvect->description = "output vector file";

  Nosup = G_define_flag ();
  Nosup->key = 'n';
  Nosup->description = "Do NOT run v.support on output file";

  Points = Vect_new_line_struct ();

  if (G_parser (argc, argv))
    exit (-1);

  if (!*(oldvect->answer))
  {
    fprintf (stderr, "%s: Command line error: missing vector file name.\n", 
    argv[0]);
    G_usage ();
    exit (-1);
  }

  if (NULL == (mapset = G_find_file2 ("dig", oldvect->answer, G_mapset ())))
  {
    sprintf (errmsg, "Could not find file '%s'", oldvect->answer);
    G_fatal_error (errmsg);
  }

  level = Vect_open_old (&oMap, oldvect->answer, mapset);

  if (level < 1)
  {
    sprintf (errmsg, "Could not open file '%s'", oldvect->answer);
    G_fatal_error (errmsg);
  }

  if (level < 2)
  {
    fprintf (stderr,"\nv.support has not been run on '%s'.\n\n",
oldvect->answer);
    exit (1);
  }

  if ( (level=Vect_open_new(&nMap, newvect->answer)) < 0)
  {
    sprintf (errmsg, "Could not open file '%s'", newvect->answer);
    G_fatal_error (errmsg);
  }

  Vect_copy_head_data(oMap.head,nMap.head);

  for (i = 1; i <= V2_num_lines(oMap); i++)
  {
    /* if (oMap.Line[i].att != 0) */
    if (V2_line_att(oMap,i) != 0)
    {
       /* if (0 < Vect_read_next_line (&oMap, Points)) */
       if (0 < V2_read_line (oMap, Points, i))
       {
         Vect_write_line(nMap,LINE,Points);
         cnt++;
       }
    }
  }

  fclose (ovect);
  fclose (nvect);
  Vect_destroy_line_struct (Points);
  Vect_close (&oMap);
  Vect_close (&nMap);

  fprintf (stderr, "Wrote %d lines.\n\n", cnt);

  if (!Nosup->answer && cnt != 0)
  {
    fprintf (stderr, "Running support now...\n\n");
    execlp ("v.support", "v.support", newvect->answer, NULL);
  }
  else
    fprintf (stderr, "You must run v.support before using this file.\n");

  exit (0);
}
