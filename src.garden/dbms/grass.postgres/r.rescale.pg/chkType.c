#include "gis.h"

chktype(tabname, colname,ctype,quelFile)
  char *tabname;
  char *colname;
  char *ctype;
  char *quelFile;
  {
    FILE *fp;
    char buf[1024];
    char *tmpfile;
    int stat;
    
	tmpfile = G_tempfile();
	if((fp = fopen(quelFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

        fprintf(fp,"SELECT (pg_type.typname) \n");
	fprintf(fp,"where pg_attribute.atttypid = pg_type.oid \n");
	fprintf(fp,"and pg_attribute.attrelid = pg_class.oid \n");
	fprintf(fp,"and pg_class.relname = \"%s\" \n",tabname);
	fprintf(fp,"and pg_attribute.attname = \"%s\" \n",colname);
	fclose(fp);
	
	sprintf(buf,"psql -ntq -c \" ` cat %s ` \" %s > %s",
                 quelFile, G__getenv("PG_DBASE"), tmpfile);
/*	printf("psql -ntq -c \" ` cat %s ` \" %s > %s\n",
                 quelFile, G__getenv("PG_DBASE"), tmpfile);
*/		
	system(buf);
	unlink(fp);


	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }
	while(!feof(fp))
	   if(fgets(buf,sizeof(buf),fp))
	     {
	       sscanf(buf,"%s",ctype);
	       if (!((strncmp(ctype,"int",3)) || (strncmp(ctype,"float",5))))
		 {
		   fprintf(stderr,"Stats: Numeric type only (%s).\n",
			   colname);
		   exit(-1);
		 };
	     };
	fclose(fp);
  }
