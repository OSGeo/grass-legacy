#include "gis.h"
#include "stats.h"
#include "infx.h"
#define LINE 80
#define HEADER "coltype"

chktype(tabname, colname, sqlFile)
  char *tabname;
  char *colname;
  char *sqlFile ;
  {
    FILE *fp;
    char buf[1024];
    char *tmpfile;
    int stat;

	stat = 0;


           
	tmpfile = G_tempfile() ;
	if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

        fprintf(fp,"%s '%s'", CHKTYP,colname);
	fclose(fp);

        sprintf(buf,"isql %s %s > %s",
		G__getenv("DATABASE"),sqlFile,tmpfile);

	system(buf);
	unlink(sqlFile);

	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }
	while(!feof(fp))
	   if(fgets(buf,sizeof(buf),fp))
		if ( (strlen(buf) > 1) && (strncmp(buf,HEADER,7) != 0) ) {
		   G_squeeze(buf);
		   if ( ! (atoi(buf) > 0 && atoi(buf) <= 5 ) ) {
 			   fprintf(stderr,"Stats: Numeric type only (%s).\n",
				   colname);
			   exit(-1);
		   }
		}
        stat = infxStats(tabname, colname, sqlFile); 

	unlink(tmpfile);

	return(stat);
   }
