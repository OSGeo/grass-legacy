
/*---------------------------------------------------------*
 *               AGNPS/GRASS Interface Project             *
 *  Developed in the Agriculture Engineering Department    *
 *                at Purdue University                     *
 *                        by                               *
 *         Raghavan Srinivasan and Bernard Engel           *
 *                                                         *
 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
 *   permission is granted, this material shall not be     *
 *   copied, reproduced or coded for reproduction by any   *
 *   electrical, mechanical or chemical processes,  or     *
 *   combinations thereof, now known or later developed.   *
 *---------------------------------------------------------*/

int display_new (map_name, label, type)
  int type;			/* indicate wheather to Overlay or Dcell */
  char *map_name, *label;
{
  char *mapset, buf[512];
  extern char *G_find_file ();
  extern char *sprintf ();
  int remove_scale (), win_label_erase (), win_label ();
  int G_fatal_error ();
  int D_translate_color (), Derase (), Dcell ();
  int R_open_driver (), R_standard_color (), R_close_driver ();


  mapset = G_find_file ("cell", map_name, "");
  if (!mapset)
  {
    sprintf (buf, "%s Cell Map mapset not found to open", map_name);
    G_fatal_error (buf);
  }

  if (type == 2)
  {
    remove_scale ();
    win_label_erase ();
  }

  R_open_driver ();

  if (type == 1)
  {
    R_standard_color (D_translate_color ("black"));

    Derase ("black");
  }

  Dcell (map_name, mapset, type - 1);

  R_close_driver ();


  win_label (label);
  return 0;
}
