#include "gis.h"

read_sites (name)
    char *name;
{
    char *mapset=NULL;
    FILE *fd;
    double east, north;
    char *desc;
    char *site_map;
    static char Sname[128];
    double xx,yy,zz, ww;
    int n, c, i, d,ret;
    int count, errors, k=0;
	char    buf[500];
  Site            *site_mgt;
  Site_head       site_info;
    int dims, cat, strs, dbls;

    mapset = G_find_file2 ("site_lists", name, "");
    if(mapset != NULL && name != NULL) {
        site_map = mapset;
        strcpy(Sname, name);
    }
   /* else{
        site_map = G_ask_sites_old("enter name of sites file", Sname);
            if (site_map == NULL){
                fprintf (stderr,  "Could not find file '%s'", Sname);
                return(0);
            }
    }
*/
    fd = G_sites_open_old (name, site_map);
    if (fd == NULL){
        fprintf (stderr, "can't open sites file [%s]", Sname);
        return(0);
    }
    G_site_get_head(fd, &site_info);

  if (G_site_describe(fd, &dims, &cat, &strs, &dbls) != 0) {
      fprintf(stderr, "failed to guess format");
    }

    /* Allocate space for site structure */
    site_mgt = G_site_new_struct (cat, dims, strs, dbls);

    fprintf (stderr, "Reading sites map (%s) ...", name);
  while((ret=G_site_get(fd, site_mgt)) != -1) {
	if( -2 == ret) fprintf(stderr, "Bad format");
    xx=(double)site_mgt->east;
    yy=(double)site_mgt->north;
    zz=(double)site_mgt->dim[0];
    ww=(double)site_mgt->dbl_att[0];

    count = errors = 0;
        newpoint(ww,zz,xx,yy);
}
    G_site_free_struct(site_mgt);
    fclose (fd);
    fprintf (stderr, "\n");

    if (errors)
    {
	fprintf (stderr, "Warning: %s - %sdid not contain %svalid elevation values\n",
		name, count?"some sites":"", count?"":"any");
    }
}
