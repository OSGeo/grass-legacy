#include "gis.h"
#include "krig.h"
#include "site.h"

int read_sites (char *name)
{
    char *mapset;
    FILE *fd;
    double east, north;
    char *desc;
    double z;
    int count;

    /* Site 5.0 handling stuff */
    Site *tmp_site;
    RASTER_MAP_TYPE rmt0;
    int n_dim, n_num, n_str;

    extern int zindx, nindx, sindx;

    mapset = G_find_sites (name,"");
    if (mapset == NULL)
      G_fatal_error("Site-list not found\n");

    fd = G_sites_open_old(name,mapset);
    if (fd == NULL)
      G_fatal_error("Can't open site-list\n");

    if(G_site_describe(fd, &n_dim, &rmt0, &n_str, &n_num) < 0) {
      fprintf(stderr, "Could not determine parameters of site-list `%s'\n",
	      name);
      exit (1);
    }

    if( (tmp_site = G_site_new_struct( rmt0, n_dim, n_str, n_num )) == NULL)
      G_fatal_error("Could not allocate site structure");

    fprintf (stderr, "Reading sites map (%s) ...", name);
    count = 0;

    while (G_site_get(fd, tmp_site) == 0) {

      if(zindx >= 0) {
	if(zindx >= tmp_site->dim_alloc) /* Index is 0-based */
	  G_fatal_error("Site-list does not contain specified dimension\n");
	else {
	  east = tmp_site->east;
	  north = tmp_site->north;
	  z = tmp_site->dim[zindx];
	  count++;
	  newpoint(z,east,north);
	}
      }

      else if(nindx >= 0) {
	if(nindx >= tmp_site->dbl_alloc) /* Index is 0-based */
	  G_fatal_error("Site-list does not contain specified numeric field\n");
	else {
	  east = tmp_site->east;
	  north = tmp_site->north;
	  z = tmp_site->dbl_att[nindx];
	  count++;
	  newpoint(z,east,north);
	}
      }

      else {
	if(nindx >= tmp_site->str_alloc) /* Index is 0-based */
	  G_fatal_error("Site-list does not contain specified character field\n");
	else {
	  east = tmp_site->east;
	  north = tmp_site->north;
	  z = atof(tmp_site->str_att[nindx]);
	  count++;
	  newpoint(z,east,north);
	}
      }

    }

    fclose (fd);
    G_site_free_struct(tmp_site);
    return 0;
}
