#define SITE_LIST

#include "gis.h"
#include "globals.h"
#include "rim.h"
#include "make.h"

/* This routine creates a site list file after a successful query.*/
/* Parameters are the site file name and optionally the field name to */
/*  use as the comment field in the site list entry for each site. */

site_list(inp_buf)
char *inp_buf;
{
struct query_record *s1;
char *p,*p1,*p2,cmd[100], filename[200], desc[82], extra_field[20];
char quote_type, catstring[100];
int extra_flag, status, cat_field_num;
FILE *fp;

if (Last_record == NULL) {  /* check for valid record query list */
        G_warning("Query or find not run or no sites selected");
        SLEEP3;
        return;
}

  /* find and extract fixed cat string, if present */
  /* quote_type will be null if no catstring */
  p1 = inp_buf;
  while (*p1 && *p1!='\"' && *p1!='\'') p1++;
  if (quote_type = *p1++) {
    p2 = catstring;
    while (*p2 = *p1++)
       if (*p2++  == quote_type) {*(p2-1) = '\0'; break;}
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

    /* get category decription field name and number, if it exists */
      G_tolcase(extra_field);

      /* search for a field name that matches */
      cat_field_num=0;
      while (cat_field_num<MAX_FIELDS && strcmp(Field_info[cat_field_num].column_name,extra_field)!=0)
        cat_field_num++;
      if (cat_field_num>=MAX_FIELDS) {
       if (!quote_type) {
        fprintf(Outfile,
                 "Unknown field name: %s.  No action taken.\n", extra_field);
        return(-1);
       }
      }
    }
sprintf(filename,"%s/%s/site_lists/%s",G_location_path(),G_mapset(),p);

        /* open the file to be sure it's there */

if ((fp=G_fopen_sites_old(p,G_mapset()))==NULL)
        if ((fp=G_fopen_sites_new(p)) == NULL) {
                G_warning("Can't open requested site file in current mapset.");
                SLEEP3;
                return (-1);
        }
fclose(fp); /* close it */

if ((fp=fopen(filename,"a"))==NULL)  /* open it for real */
                G_fatal_error("In site_list(): Can't reopen site file.");

fprintf(fp,"\n# Next %d sites added by v.db.rim from %s data base: %s\n",
                (Last_record-Record_list)+1, File_name, G_date() );

/* put the sites in the list */
for (s1=Record_list; s1<=Last_record; s1++)
 {
        *desc = '\0';
        if (quote_type)
                sprintf(desc,"%s",catstring);
        else
        if (extra_flag==0 ||
                !strcmp(extra_field,Field_info[Sequence_field].column_name))
                sprintf(desc,"#%d",s1->record_number);
        else
        if (!strcmp(extra_field,Field_info[North_field].column_name))
                sprintf(desc,"#%.2f",s1->north);
        else
        if (!strcmp(extra_field,Field_info[East_field].column_name))
                sprintf(desc,"#%.2f",s1->east);
        else {
                sprintf(cmd, "select from data where %s = %d",
                Field_info[Sequence_field].column_name, s1->record_number);
                if ((status=crim(DATA_TABLE, cmd))>0)
                        rim_error(status);
                else if (status==RIM_EOT) {
                        sprintf(cmd, "Unable to find %s %d in database.",
                        Field_info[Sequence_field].column_name, s1->record_number);
                        G_warning(cmd);
#ifdef DBSITES
        SLEEP3;
#endif
                        }
                        else {
                                crimdm_w_err(DATA_TABLE, GET, Rim_buffer);
                                fill_values();
                                *desc = '#';
                                val_to_str(desc+1, cat_field_num);
                        }

        }

                G_strip(desc);
/* Use next line for GRASS 3.1 and later */
/*              G_put_site(fp,s1->east,s1->north,desc); */
/* and remove the following line */
                fprintf(fp,"%.2f|%.2f|%s\n",s1->east,s1->north,desc?desc:"");
        }  /* end of for loop for all sites */
fclose(fp);
}
