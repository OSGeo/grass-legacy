/* 
 * Writes a sites file from 2 Point2 lists, using x & y from first 
 * list and y from second list as third dimension in site. 
 *
*/

#include "enforce.h"
#include "gis.h"
#include "site.h"

write_xyz_sites(pgxypts, pgpts, npts, outsite, depth)
Point2 *pgxypts, *pgpts;
int npts;
char *outsite;
double depth;
{
FILE *sitefd;
char errbuf[160];
Site *s;
int i;

Site_head shead;
DateTime dt;
struct TimeStamp ts;
int tz, ret;

    if ((sitefd = G_fopen_sites_new(outsite)) == NULL)
    {
        sprintf(errbuf,"Not able to open sitesfile for [%s]", outsite);
        G_fatal_error(errbuf);
    }

    if(NULL == (s = G_site_new_struct(-1,3,0,0)))
        G_fatal_error("memory allocation failed for site");

    shead.name = G_store(outsite);
    shead.desc = G_store(G_recreate_command());

    datetime_set_type (&dt, DATETIME_ABSOLUTE, DATETIME_YEAR,
		       DATETIME_SECOND, 0);
    datetime_get_local_time (&dt);
    datetime_get_local_timezone (&tz);
    datetime_set_timezone (&dt, tz);

    G_set_timestamp (&ts, &dt);
    shead.time = &ts;
    shead.form = G_store("|||");
    shead.labels = G_store("Easting|Northing|Elevation|");
    G_site_put_head (sitefd, &shead);
    G_free (shead.name);
    G_free (shead.desc);
    G_free (shead.form);
    G_free (shead.labels);

    for(i=0; i<npts; i++){
	s->east = pgxypts[i][0];
        s->north = pgxypts[i][1]; 
	s->dim[0] = pgpts[i][1] - depth; 
	G_site_put(sitefd, s);
    }

    G_site_free_struct (s);
    fclose(sitefd);
    
    return(1);

}

