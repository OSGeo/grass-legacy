/*  Search Type 7 records for Points with required CFCC's.
    Write each to GRASS site_list or vector file.
*/
#include "gis.h"
#include "Vect.h"
#include "globals.h"

do_points()
{
int n,both_kinds;
char  buff[100];

if (feature_type[1][0] != 'P') return 0; /* don't do anything */
open_site_file();
tig_open('7');
n = 0;
both_kinds = both_conds();
is_a_match("~!~start!","");
while (get_tiger_record(tiger[7].fp,'7',buff+1)!=EOF)
  if (point_yes(buff) && good_one('P',buff,both_kinds)) {
      save_site(buff);
      n++;
  }
tig_close('7');
close_site_file(n);
return n;
}

/* check for a point record (missing lat-long is not a point) */
point_yes(buf)
char *buf;
{
  return ((buf[59] == ' ')? 0: 1);
}

open_site_file()   /* open site list */ 
{
char dat[20];
FILE *fp1;

if (sitefile==NULL) {
    if ((sitefile=G_fopen_sites_new(site_name))==NULL)
      G_fatal_error("Could not open requested site list.");
    else {
      fp1 = popen("date +%m/%d/%y", "r"); /* get the current date */
      fscanf(fp1, "%10s", dat);
      pclose(fp1);
      fprintf(sitefile,"desc| Census Landmark points extracted %s\n",dat);
    }
  return 1;
  }
}

save_site(buf)
char *buf;
{
char comment[100];
double x,y;

get_point_coords(buf+55,&x, &y);

if (sitefile) { /*comment = #landmark# CFCC Feature name */
  sprintf(comment,"#%.10s %.3s %.30s",buf+11,buf+22,buf+25);
  G_squeeze(comment);          /* remove trailing blanks */
  G_put_site(sitefile, x, y, comment);
}
else
  G_fatal_error("No site file open to write to.");
}

close_site_file(n)
int n;
{
  if (sitefile!=NULL){
    fclose(sitefile);
    if (!n) G_remove("site_lists",site_name);
    sitefile = NULL;
  }
}

