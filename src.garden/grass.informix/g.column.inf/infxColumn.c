#include "gis.h"
#include "column.h"
#define LEN 20
#define LINE 80
#define HEADER "colname"

infxColumn(tabname,flag, sqlFile)
  char *tabname;
  int   flag;
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

           
        sprintf(buf,"isql %s %s > %s\n",
		G__getenv("DATABASE"),sqlFile,tmpfile);
        
        system(buf);
	if((fp = fopen(tmpfile,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file.\n");
	    exit(-1);
           }

	fprintf(stderr,"The following columns are defined in table: %s\n\n",tabname);

	if (flag == 0)  {                 /* 4 column list */
    	   i = 0;
	   *prev_cname = 0;
           while (G_getl(buf,sizeof(buf),fp)) {
		 G_squeeze(buf); 
		 if (*buf == 0) continue;
 		 if(sscanf(buf,"%s",cname) != 1) continue;
		 if  (strncmp(cname,HEADER,strlen(HEADER)) != 0)  {
		    if (strcmp(cname,prev_cname) == 0) continue; else strcpy(prev_cname,cname);;
		    if (i==3) {
             	           printf("%s\n",cname);
			   i=0;
			}
			else {
				printf("%-*s",LEN,cname);  /* - to left just, *=use LEN for size */
			   	i++;
			}
		    }
		}
          }   /* flag == 0 */
	else
	  *prev_cname = 0;
          while (G_getl(buf,sizeof(buf),fp))
		if(sscanf(buf,"%s %d %d", cname, &ctype, &clength)  == 3)
			parseType(cname, ctype, clength);

        printf("\n");
        fclose(fp);
	unlink(tmpfile);
		
}


parseType(cname, ctype, clength)
  char *cname;
  int ctype, clength;

{
	char *vtype;


	switch(ctype) {
		case 0: vtype  = "char"; break;
		case 1: vtype  = "smallint"; break;
		case 2: vtype  = "integer"; break;
		case 3: vtype  = "unknown"; break;
		case 4: vtype  = "unknown"; break;
		case 5: vtype  = "decimal"; break;
		case 6: vtype  = "unknown"; break;
		case 7: vtype  = "date"; break;
		case 256: vtype = "char (no nulls)"; break;
		case 257: vtype = "smallint (no nulls)"; break;
		case 258: vtype = "integer (no nulls)"; break;
		case 259: vtype = "unknown (no nulls)"; break;
		case 260: vtype = "unknown (no nulls)"; break;
		case 261: vtype = "decimal (no nulls)"; break;
		case 262: vtype = "serial (no nulls)"; break;
		default: return; break;
	}

	/* Informix identifies decimal data types using 2**8x signif digits + precision
	   therefore decimal(5,2)=1282=2**8x5+2
	*/
	if(ctype == 5 || ctype  == 261) 
		fprintf(stdout,"%20s	%20s	%d,%d\n",cname, vtype, clength/256, clength%256);
	 else
		fprintf(stdout,"%20s	%20s	%d\n",cname, vtype, clength);
}
