#include "gis.h"

/* return RGB for given category */
/* works for null values too */
int G_get_color (CELL n, int *red, int *grn, int *blu, struct Colors *colors)
{
    CELL cat;
    unsigned char r, g, b, set;

    cat = n;
    G_lookup_colors (&cat, &r, &g, &b, &set, 1, colors);

    *red = (int) r;
    *grn = (int) g;
    *blu = (int) b;

    return (int)set;
}

int G_get_raster_color (void *rast,
    int *red, int *grn, int *blu,
    struct Colors *colors, RASTER_MAP_TYPE map_type)
{
    unsigned char r, g, b, set;

    G_lookup_raster_colors (rast, &r, &g, &b, &set, 1, colors, map_type);

    *red = (int) r;
    *grn = (int) g;
    *blu = (int) b;

    return (int)set;
}

int G_get_c_raster_color (CELL *rast,
    int *red, int *grn, int *blu, struct Colors *colors)
{
    unsigned char r, g, b, set;

    G_lookup_raster_colors (rast, &r, &g, &b, &set, 1, colors, CELL_TYPE);

    *red = (int) r;
    *grn = (int) g;
    *blu = (int) b;

    return (int)set;
}

int G_get_f_raster_color (FCELL *rast,
    int *red, int *grn, int *blu, struct Colors *colors)
{
    unsigned char r, g, b, set;

    G_lookup_raster_colors (rast, &r, &g, &b, &set, 1, colors, FCELL_TYPE);

    *red = (int) r;
    *grn = (int) g;
    *blu = (int) b;

    return (int)set;
}

int G_get_d_raster_color (DCELL *rast,
    int *red, int *grn, int *blu, struct Colors *colors)
{
    unsigned char r, g, b, set;

    G_lookup_raster_colors (rast, &r, &g, &b, &set, 1, colors, DCELL_TYPE);

    *red = (int) r;
    *grn = (int) g;
    *blu = (int) b;

    return (int)set;
}

int G_get_null_value_color (int *red, int *grn, int *blu,
    struct Colors *colors)
{
  if(colors->null_set)
  {
      *red = (int) colors->null_red;
      *grn = (int) colors->null_grn;
      *blu = (int) colors->null_blu;
  }
  else if(colors->undef_set)
  {
      *red = (int) colors->undef_red;
      *grn = (int) colors->undef_grn;
      *blu = (int) colors->undef_blu;
  }
  else
      *red = *blu = *grn = 255; /* white */

  return 0;
}

int G_get_default_color (int *red, int *grn, int *blu,
    struct Colors *colors)
{
  if(colors->undef_set)
  {
      *red = (int) colors->undef_red;
      *grn = (int) colors->undef_grn;
      *blu = (int) colors->undef_blu;
  }
  else
      *red = *blu = *grn = 255; /* white */

  return 0;
}
