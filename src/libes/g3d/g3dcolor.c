#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "gis.h"
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/


/*!
 * \brief 
 *
 *  Removes the primary and/or secondary color file. See <em>G_remove_colr</em> for
 * details.
 *
 *  \param name
 *  \return always 0
 */

int
G3d_removeColor (name) /* adapted from G_remove_colr */

     char *name;

{
  char buf[200], secondary[500], buf2[200], xname[512], xmapset[512];

  if (G__name_is_fully_qualified (name, xname, xmapset)) {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, xname);
    sprintf (buf2, "%s@%s", G3D_COLOR_ELEMENT, xmapset); /* == color@mapset */
  } else {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, name);
    sprintf (buf2, "%s", G3D_COLOR_ELEMENT);
  }

  G_remove (buf, buf2);

  sprintf (secondary, "%s/%s/%s", 
	   G3D_DIRECTORY, G3D_COLOR2_DIRECTORY, G_mapset());
  G_remove (secondary, name);

  return 0;
}
/*---------------------------------------------------------------------------*/


/*!
 * \brief 
 *
 *  Reads color file for map <em>name</em> in <em>mapset</em> into the
 * <em>colors</em> structure.  See <em>G_read_colors</em>
 * (Raster_Color_Table) for details and return values.
 *
 *  \param name
 *  \param mapset
 *  \param colors
 *  \return int
 */

int
G3d_readColors (name, mapset, colors) /* adapted from G_read_colors */

     char *name;
     char *mapset;
     struct Colors *colors;

{
  char buf[512], buf2[200];
  char *err;
  char xname[512], xmapset[512];
  struct FPRange drange;
  DCELL dmin, dmax;

  G_init_colors (colors);

  if (G__name_is_fully_qualified (name, xname, xmapset)) {
    if (strcmp (xmapset, mapset) != 0) return -1;
    name = xname;
  }

  sprintf (buf,"%s/%s/%s", G3D_DIRECTORY, G3D_COLOR2_DIRECTORY, mapset);
  if (G_read_colors (name, G_mapset(), colors) >= 0) return 1;

  G_mark_colors_as_fp (colors);

/* now look for the regular color table */
  if (G__name_is_fully_qualified (name, xname, xmapset)) {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, xname);
    sprintf (buf2, "%s@%s", G3D_COLOR_ELEMENT, xmapset); /* == color@mapset */
  } else {
    sprintf (buf, "%s/%s", G3D_DIRECTORY, name);
    sprintf (buf2, "%s", G3D_COLOR_ELEMENT);
  }

  switch (G_read_colors (buf2, mapset, colors)) {
  case -2:
    if (G3d_readRange (name, mapset, &drange) >= 0) {
      G_get_fp_range_min_max (&drange, &dmin, &dmax);
      if(! G_is_d_null_value (&dmin) && ! G_is_d_null_value(&dmax))
	G_make_rainbow_fp_colors (colors, dmin, dmax);
      return 0;
    }
    err = "missing";
    break;
  case -1:
    err = "invalid";
    break;
  default:
    return 1;
  }

  sprintf(buf,"color support for [%s] in mapset [%s] %s", name, mapset, err);
  G_warning (buf);
  return -1;
}

/*---------------------------------------------------------------------------*/


/*!
 * \brief 
 *
 * Writes colors stored in <em>colors</em> structure into the color
 * file for map <em>name</em> in <em>mapset</em>.  See <em>G_write_colors</em>
 * (Raster_Color_Table) for
 * details and return values.
 *
 *  \param name
 *  \param mapset
 *  \param colors
 *  \return int
 */

int
G3d_writeColors (name, mapset, colors) /* adapted from G_write_colors */

     char *name;
     char *mapset;
     struct Colors *colors;

{
  char element[512], buf[512], buf2[200];
  char xname[512], xmapset[512];
  FILE *fd;
  int stat;

  if (G__name_is_fully_qualified (name, xname, xmapset)) {
    if (strcmp (xmapset, mapset) != 0) return -1;
    name = xname;
  }

/*
 * if mapset is current mapset, remove colr2 file (created by pre 3.0 grass)
 *    and then write original color table
 * else write secondary color table
 */

  sprintf (element,"%s/%s/%s", G3D_DIRECTORY, G3D_COLOR2_DIRECTORY, mapset);
  if (strcmp (mapset, G_mapset()) == 0) {
    G_remove (element, name); /* get rid of existing colr2, if any */

    if (G__name_is_fully_qualified (name, xname, xmapset)) {
      sprintf (buf, "%s/%s", G3D_DIRECTORY, xname);
      sprintf (buf2, "%s@%s", G3D_COLOR_ELEMENT, xmapset); /* == color@mapset */
    } else {
      sprintf (buf, "%s/%s", G3D_DIRECTORY, name);
      sprintf (buf2, "%s", G3D_COLOR_ELEMENT);
    }

    if (! (fd = G_fopen_new (buf, buf2))) return -1;
  } else {
    if (! (fd = G_fopen_new (element, name))) return -1;
  }

  stat = G__write_colors (fd, colors);
  fclose (fd);
  return stat;
}
