#include <stdio.h>
#include <dirent.h>

int getdbname(dbtemp)
char *dbtemp;
{
char *temp_file;
char dbname[18];
char sysbuf[600];
FILE *fpin;

temp_file = G_tempfile();
/*sprintf(sysbuf, "psql -ntq -c 'SELECT ( pg.datname ) from pg in pg_database sort by datname' >%s",temp_file);*/
sprintf(sysbuf, "psql -nqt -l >%s",temp_file);
system(sysbuf);

if((fpin = fopen(temp_file,"r")) == NULL) {
            fprintf(stderr, "File read error on temporary file (output)\n");
            exit(-1);
           }
while(fscanf(fpin,"%s",dbname)!=EOF) 
	if (!strcmp(dbname, dbtemp))
		{
		fclose(fpin) ;
		return(0);
		}

fclose(fpin) ;
return(1);
}


/*
int getdbname(dbtemp, dbptr )
     char *dbtemp, *dbptr; 
  {
    char tmp[80];
    DIR *opendir();
    struct dirent *readdir();
    DIR *dirp;
    struct dirent *dp;

    sprintf(tmp,"%s",dbptr);

    if(dirp = opendir(dbtemp))
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) 
	if (!strcmp(dp->d_name, tmp))
	   return(0);
    return(1);
  }
*/
