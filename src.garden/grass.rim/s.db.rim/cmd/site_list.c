#define SITE_LIST

#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"

/* This routine creates a site list file after a successful query.*/
/* Parameters are the site file name and optionally the field name to */
/*   use as the comment field in the site list entry for each site */

site_list(inp_buf)
char *inp_buf;
{
struct query_site *s1;
char *p,*p1, cmd[100], filename[200], desc[82], extra_field[20];
int count, split, tempint;
int extra_flag, i, f, status;
FILE *fp;

if (Last_site == NULL) {  /* check for valid site list */
        G_warning("Query or find not run or no sites selected");
        SLEEP3;
        return;
}
G_squeeze(inp_buf);
if ((p=index(inp_buf,' '))==NULL) {
        G_warning(".site_list needs a site list name parameter.");
        SLEEP3;
        return (-1);
}
p++;  /* point to file name */

p1 = p + strcspn(p," \t"); /* see if there is another piece of text */
if (! *p1) extra_flag = 0;
        else {
                extra_flag = 1;
  /* set null on site list name and point to field name */
                *p1++ = '\0';
                strncpy(extra_field, p1,17);
        /* be sure field exists */
                G_tolcase(extra_field);
                for (i=0, f=0; i<Field_num; i++)
                        if (!strcmp(extra_field,Field_info[i].column_name))
                                {f = 1; break;}
                if (!f) {
                        G_warning(".site_list comment field is not one in current data base.");
                        SLEEP3;
                        return (-1);
                        }
        }
sprintf(filename,"%s/%s/site_lists/%s",G_location_path(),G_mapset(),p);

        /* open the file to be sure it's there */

if ((fp=G_fopen_sites_old(p,G_mapset()))==NULL)
                if ((fp=G_fopen_sites_new(p)) == NULL)
                        G_fatal_error("Can't open requested site file in current mapset.");
fclose(fp); /* close it */

if ((fp=fopen(filename,"a"))==NULL)  /* open it for real */
                G_fatal_error("Can't reopen site file.");

fprintf(fp,"\n# Next %d sites added by s.db.rim from %s data base: %s\n",
                (Last_site-Site_list)+1, File_name, G_date() );

/* put the sites in the list */
for (s1=Site_list; s1<=Last_site; s1++)
 {
        *desc = '\0';
        if (extra_flag==0 ||
		    !strcmp(extra_field,Field_info[Site_field].column_name))
                sprintf(desc,"#%d",s1->site_number);
        else
        if (!strcmp(extra_field,Field_info[North_field].column_name)) {
	   str_from_e_n(desc,North_field,s1->north);
        }
        else
        if (!strcmp(extra_field,Field_info[East_field].column_name)) {
	   str_from_e_n(desc,East_field,s1->east);
        }
        else {
                sprintf(cmd, "select from data where %s = %d",
                Field_info[Site_field].column_name, s1->site_number);
                if ((status=crim(DATA_TABLE, cmd))>0)
                        rim_error(status);
                else if (status==RIM_EOT) {
                        sprintf(cmd, "Unable to find %s %d in database.",
                        Field_info[Site_field].column_name, s1->site_number);
                        G_warning(cmd);
#ifdef DBSITES
        SLEEP3;
#endif
                        }
                        else {
                                crimdm_w_err(DATA_TABLE, GET, Rim_buffer);
                                fill_values();
                                for (i=0; i<Field_num; i++)
                                        if (!strcmp(extra_field,Field_info[i].column_name))
                                        {
                                        *desc = '#';
                                        val_to_str(desc+1, i);
                                        G_strip(desc);
                                        break;
                                        }
                        }

        }
        G_put_site(fp,s1->east,s1->north,desc);
  }  /* end of for loop for all sites */
fclose(fp);
}

str_from_e_n(str,i,val)
int i; double val; char *str;
{
char format[40];
sprintf(format,"#%%%d.%dlf",Field_info[i].length,Field_info[i].next_field[1]);
sprintf(str, format, val);
}
