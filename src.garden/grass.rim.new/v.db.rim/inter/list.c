#define LIST
/*  To list the available rim data bases */
#include "gis.h"
#include "globals.h"
#include "rim.h"
#include <sys/types.h>
#include <sys/dir.h>

list()
{
  DIR *dp;
  struct direct *dirp;
  char cmd[400],buf[200];
  char *mapset;
  char *p1,*p2;
  int n,count,print;

  fprintf(Outfile,
          "\nLOCATION <%s>.  AVAILABLE v.db.rim DATA BASES:", G_location());

  for (n=0; mapset=G__mapset_name(n); n++) {
    count = 0;

    sprintf(cmd,"%s/%s/%s",G_location_path(),mapset,RIM_SUB_DIR);
    if ((dp = opendir(cmd))!=NULL) {
      while ((dirp=readdir(dp))!=NULL) {
        strcpy(buf, dirp->d_name);
        p1 = buf;               /* find character after last . in string */
        p2 = 0;
        while (*p1){
          if (*p1 == '.') p2 = p1+1;
          p1++;
        }
        if (!p2) continue;
        p1 = p2;
        /* there should be a "rimdb1" on a good line */
        print = 0;
        if (strncmp(p1,"rimdb1",6)==0) print=1;
        if (print) {
          /* get just first part of file name */
          p1--;                 /*backup and put a null in place of the '.' */
          *p1 = '\0';
          count++;
          if (count==1) {
    fprintf(Outfile,"\n\nMAPSET <%s>",mapset);
    fprintf(Outfile,"%s",strcmp(mapset,G_mapset())?"    (READ-ONLY)":"");
                }
          if( ! ((count-1)%5)) fprintf(Outfile,"\n");
          fprintf(Outfile,"%-15s",buf);
        }
      }
      closedir(dp);
    }
  }                             /* end of for loop */
  fprintf(Outfile,"\n\n");
}
