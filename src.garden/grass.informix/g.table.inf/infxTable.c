#include "gis.h"
#define TABLEN 20

infxTables(sqlFile)
  char *sqlFile;
  {
    FILE *fp;
    int buflen, i;
    char buf[1024];
    char *tmpfile;

    i = 0;
    tmpfile = G_tempfile();

           
        sprintf(buf,"isql %s %s > %s",
		G__getenv("DATABASE"), sqlFile, tmpfile);
        
        system(buf);

	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }

	printf("The following tables are available in database: %s\n\n",
		G__getenv("DATABASE") );

	/* use G_getl to read line and toss CR check lib/gis for call seq  */
        while (!feof(fp) )
	  if(fgets(buf,TABLEN,fp)) 
		if ( (strlen(buf) > 1) && (strncmp(buf,"tabname",7) != 0) ) {
			if (i==2) {
			   G_squeeze(buf);
             	           printf("%s\n",buf);
			   i=0;
			}
			else {
			   delcr(buf);
			   i++;
			}
		}
        printf("\n");
        fclose(fp);
	unlink (tmpfile);
		
}

