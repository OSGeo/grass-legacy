#include "gis.h"
#include "globals.h"

do_lines()
{
int i, nrec, both_kinds;
char *tmpfile, buff[300], cmd[500];
FILE *fp1;

if (feature_type[3][0] != 'L') return 0;

both_kinds = both_conds(); /* init for good_one() */
is_a_match("~!~start!","");   /* init for good_one() */

fp1 = fopen(tmpfile=G_tempfile(),"w");
nrec = 0;
tig_open('1');
while (get_tiger_record(tiger[1].fp,'1',buff+1)!=EOF) {
  if (good_one('L',buff,both_kinds)) { /* got line of right cat/fename */
    fprintf(fp1,"%.10s\n",buff+6);
    nrec++;
  }
}
fclose(fp1);
fprintf(stderr,"%d records matching requested CFCC's found for map %s \n",
     nrec, vect_name);
tig_close('1');
if (nrec) {
  sprintf(cmd,"v.in.tig.basic t1=%s t2=%s out=%s tlid=%s",
             tiger[1].name, tiger[2].name, vect_name, tmpfile);
  if (proj == PROJECTION_LL){
    sprintf(buff," spheroid=%s zone=%d",sphere, zone);
    strcat(cmd,buff);
  }
  G_system(cmd);
  sprintf(cmd,"v.support map=%s option=build",vect_name);
  G_system(cmd);
}
unlink(tmpfile);

return nrec;
}
