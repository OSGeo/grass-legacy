#include "gis.h"
#include "column.h"
#define LEN 20
#define LINE 80
#define HEADER "colname"

oraColumn(tabname,sqlFile)
  char *tabname;
  char *sqlFile;
  {
    FILE *fp;
    int i, hit;
    char buf[1024];
    char cname[1024];
    char prev_cname[1024];
    char *tmpfile;
    int ctype;
    int clength;

    tmpfile = G_tempfile();

           
        sprintf(buf,"sqlplus -s %s @%s > %s\n",
		G__getenv("DATABASE"),sqlFile,tmpfile);
        
        system(buf);
	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file.\n");
	    exit(-1);
           }

	fprintf(stderr,"The following columns are defined in table: %s\n\n",tabname);

        while (G_getl(buf,sizeof(buf),fp)) {
             	fprintf (stdout,"%s\n",buf);
       		} 
	fclose(fp);
	unlink(tmpfile);
		
}

