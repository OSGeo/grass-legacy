/* updated to PostgreSQL 6.3 by Markus Neteler 8/98 */

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
    char buf[1024], ctype[20];
    char *tmpfile;
    int stat;

	stat = 0;


           
	tmpfile = G_tempfile() ;
	if((fp = fopen(sqlFile,"w")) == NULL) {
            fprintf(stderr, "File write error on temporary file\n");
	    exit(-1);
           }

        fprintf(fp,"select pg_type.typname \n");
	fprintf(fp,"where pg_attribute.atttypid = pg_type.oid \n");
	fprintf(fp,"and pg_attribute.attrelid = pg_class.oid \n");
	fprintf(fp,"and pg_class.relname = \"%s\" \n",tabname);
	fprintf(fp,"and pg_attribute.attname = \"%s\" \n",colname);
	fclose(fp);

/*
        sprintf(buf,"isql %s %s > %s",	
	        G__getenv("DATABASE"),sqlFile,tmpfile);
*/
        sprintf(buf,"psql -tnq -c \" ` cat %s ` \" %s > %s",
                 sqlFile, G__getenv("PG_DBASE"), tmpfile);
		
	system(buf);
	unlink(sqlFile);

	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file\n");
	    exit(-1);
           }
	while(!feof(fp))
	   if(fgets(buf,sizeof(buf),fp))
	     /*if ( (strlen(buf) > 1) && (strncmp(buf,HEADER,7) != 0) ) {
		   G_squeeze(buf);
		   if ( ! (atoi(buf) > 0 && atoi(buf) <= 5 ) ) {
 			   fprintf(stderr,"Stats: Numeric type only (%s).\n",
				   colname);
			   exit(-1);
		   }
		}
	  */
	     {
	       sscanf(buf,"%s",ctype);
	       if (!((strncmp(ctype,"int",3)) || (strncmp(ctype,"float",5))))
		    {
		      fprintf(stderr,"Stats: Numeric type only (%s).\n",
			      colname);
		      exit(-1);
		    };
	     };
        stat = infxStats(tabname, colname, sqlFile, ctype); 

	unlink(tmpfile);

	return(stat);
   }
