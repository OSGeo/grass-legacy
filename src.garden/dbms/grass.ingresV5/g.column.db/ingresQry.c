/*
 * ingresQry - Process the output from the SQL query
 *
 * Katarina Johnsson 930415
 */
#include <stdio.h>
#include "ingresUtils.h"
#include "gis.h"
#define LEN 20
#define LINE 80
#define HEADER "column_name"

ingresQry(tabname,flag, sqlFile)
  char *tabname;
  int   flag;
  char *sqlFile;
  {
    FILE *fp;
    int i, colFlag=0, rowCnt=0;
    char buf[1024];
    char cname[1024];
    /*char prev_cname[1024];*/
    char *tmpfile;
    int ctype;
    int clength;

    const char col1[] = "column";
    const char col2[] = "datatype";
    const char col3[] = "length";

    tmpfile = G_tempfile();

           
        sprintf(buf,"sql -s %s < %s > %s\n",
		G__getenv("DATABASE"),sqlFile,tmpfile);
        
        system(buf);
	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file.\n");
	    exit(-1);
           }

	fprintf(stderr,"\nThe following columns are defined in table: %s\n\n",tabname);

	if (flag==0) {            
	   while (G_getl(buf,sizeof(buf),fp)) 
		printf("%s\n",buf);
	}
	else { /* Verbose listing */
           while (G_getl(buf,sizeof(buf),fp)) {
		 ingresUnTable(buf);
 		 G_squeeze(buf);
		 if (*buf == 0) continue;

		 if (colFlag==0) { /*column headers */
			printf("|%-20s|%-20s|%-10s|\n",col1,col2,col3);
			printf("|");
			for(i=0;i<52;i++)
				printf("-");
			printf("|\n");
			colFlag=1;
			continue;
		 }
		 if(sscanf(buf,"%s %d %d",cname,&ctype,&clength) == 3) { 
			parseType(cname,ctype,clength);
			rowCnt++;
		 }
	   }
	   printf("|");
	   for(i=0;i<52;i++)
		printf("-");
	   printf("|\n");
           printf("(%d rows)\n\n",rowCnt);
	}
        fclose(fp);
	unlink(tmpfile);
		
}


parseType(cname, ctype, clength)
  char *cname;
  int ctype, clength;

{
	char *vtype;


	switch(ctype) {
		case 30: vtype  = "integer"; break;
		case 31: vtype  = "float"; break;
		case 32: vtype  = "character"; break;
		case 37: vtype  = "text"; break;
		case 3: vtype  = "date"; break;
		case 5: vtype  = "money"; break;
		default: return; break;
	}

		fprintf(stdout,"|%-20s|%-20s|%-10d|\n",cname, vtype, clength);
}
