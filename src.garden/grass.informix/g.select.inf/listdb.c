#include <stdio.h>
#include <dirent.h>
#define EXT ".dbs"
#define FS '.'

int listdb(dbtemp)
     char *dbtemp; 
  {
    DIR *opendir();
    struct dirent *readdir();
    DIR *dirp;
    struct dirent *dp;
    int j;
    unsigned short i;
    char dbname[1024];


    G_squeeze(dbtemp);
    if(dirp = opendir(dbtemp))
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) 
	if (dp->d_name[dp->d_namlen-strlen(EXT)] == FS) {
		j = 0;
		for(i=dp->d_namlen-strlen(EXT); i < dp->d_namlen; i++) {
			dbname[j] = dp->d_name[i];
			j++;
		}
		dbname[j] = '\0';
		if(!strcmp(dbname,EXT))
			fprintf(stdout,"	%s\n",dp->d_name);
	}
    return(1);
  }

