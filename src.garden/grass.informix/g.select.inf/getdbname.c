#include <stdio.h>
#include <dirent.h>

int getdbname(dbtemp, dbptr )
     char *dbtemp, *dbptr; 
  {
    char tmp[80];
    DIR *opendir();
    struct dirent *readdir();
    DIR *dirp;
    struct dirent *dp;

    sprintf(tmp,"%s.dbs",dbptr);

    if(dirp = opendir(dbtemp))
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) 
	if (!strcmp(dp->d_name, tmp))
	   return(0);
    return(1);
  }

