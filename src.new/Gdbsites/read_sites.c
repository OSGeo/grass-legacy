/* To add records to data base with locations from a site_list */

#include "gis.h"
#include "globals.h"
#include "make.h"

extern int Found_site;

read_sites(buffer)
char *buffer;
{
char *file, *mapset,*text_field;
double east,north;
char *desc,*p;
int style_flag, count, site, text_field_num,text_field_len;
FILE *fp;

G_squeeze(buffer);
p = buffer;   /*count the spaces */
file=0; text_field=0;
count=1;
while (*p != '\0') {
	if (*p==' ') {
		count++;
		if (count==2) file = p+1;
		if (count==3) {*p='\0';text_field = p+1;break;}
		}
	p++;}
/*
file = buffer + strcspn(buffer," \t");
*/
if (! *file) {
	G_warning("Data not added: .read_sites requires site_list name.");
	SLEEP3;
	return(-1);
	}
/*file += strspn(file, " \t"); */
G_squeeze(file);
mapset = G_find_file ("site_lists", file, "");

if (mapset == NULL) {
	G_warning
		(".read_site: Requested site file not found in any current mapset.");
	SLEEP3;
	return (-1);
	}
/* Identify the text field in the data base */
text_field_num = 0;
if (text_field) {
	G_tolcase(text_field);
	while (text_field_num<MAX_FIELDS &&
	strcmp(Field_info[text_field_num].column_name,text_field)!=0)
	text_field_num++;

	if(text_field_num>=MAX_FIELDS) {
	fprintf(Outfile,"Unknown field name for description. No data added");
	SLEEP3;
	return(-1);
	}
	if(Field_info[text_field_num].column_type != T_FIELD_CHAR) {
	G_warning(
	"Field to fill with description not a text field. No data added.");
	SLEEP3;
	return(-1);
	}
	text_field_len = Field_info[text_field_num].length - 1;
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
	if (text_field_num) {
		strncpy(Field_info[text_field_num].value,
			desc,text_field_len);
		Field_info[text_field_num].value[text_field_len] = '\0';
	}
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

