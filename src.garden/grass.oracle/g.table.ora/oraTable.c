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

           
        sprintf(buf,"sqlplus -s %s @%s > %s",
		G__getenv("DATABASE"), sqlFile, tmpfile);
        
        system(buf);

	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }

	fprintf (stdout,"The following tables are available for user: %s\n\n",
		G__getenv("DATABASE") );

	/* use G_getl to read line and toss CR check lib/gis for call seq  */
        while (!feof(fp) )
	  if(fgets(buf,TABLEN,fp)) 
		if (strlen(buf) > 1) 
             	    fprintf (stdout,"%s",buf);
        fprintf (stdout,"\n");
        fclose(fp);
	unlink (tmpfile);
		
}

