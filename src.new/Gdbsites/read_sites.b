/* To add records to data base with locations from a site_list */

#include "gis.h"
#include "globals.h"

extern int Found_site;

read_sites(buffer)
char *buffer;
{
char *file, *mapset;
double east,north;
char *desc;
int style_flag, count, site;
FILE *fp;

G_squeeze(buffer);

file = buffer + strcspn(buffer," \t");
if (! *file) {
	G_warning("Data not added: .read_sites requires site_list name.");
	SLEEP3;
	return(-1);
	}
file += strspn(file, " \t");
G_squeeze(file);
mapset = G_find_file ("site_lists", file, "");

if (mapset == NULL) {
	G_warning
		(".read_site: Requested site file not found in any current mapset.");
	SLEEP3;
	return (-1);
	}

/* Make first pass to set style */
style_flag = 1;  /* assume desc is site number */

fp = G_fopen_sites_old(file,mapset);
if (fp == NULL) {
	G_warning(".read_site: Could not open requested site_list");
	SLEEP3;
	return (-1);
	}
while (G_get_site(fp,&east,&north,&desc)!= -1) {
	G_squeeze(desc);
   if (sscanf(desc,"#%d",&site) != 1) {
		style_flag = 0;
		break;
		}
	}
fclose(fp);

/* reopen the site file */
fp = G_fopen_sites_old(file,mapset);
site = get_max_site();
add_change_init();
Found_site = TRUE;

while (G_get_site(fp,&east,&north,&desc)!= -1) {
	if (style_flag) {
		G_squeeze(desc);
		sscanf(desc,"#%d", &site);
		}
		else site++;
	*((int *) Field_info[Site_field].value) = site;
	*((double *) Field_info[East_field].value) = east;
	*((double *) Field_info[North_field].value) = north;
	add_done();
	}
fclose(fp);
}

