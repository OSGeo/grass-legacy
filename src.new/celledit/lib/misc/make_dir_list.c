/***********************************************************************

File     	:	make_dir_list.c
Function 	:	char **MakeFileList(path)
Args	 	:	    char *path; -- path to build list from

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	28 January 1990
Last Revised	:
Abstract 	:	Takes an absolute path, and builds a list 
			of file names.
Returns  	:	the list of file names,

***********************************************************************/
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
StrCmp();		/* qsort function */

char **MakeFileList(path, num)
    char *path;
    int *num;
    {
    DIR *dirp;				/* pointer to a DIR entry */
    struct dirent *dp;			/* array of directory entries */
    char **fileList;			/* the file names */
    char **currFile;			/* a counter */
    int entryCnt;			/* a test constant */

    if( (entryCnt = scandir(path, &dp, NULL, NULL)) == NULL)
	return((char **)NULL);

    fileList = (char **) G_calloc(entryCnt, sizeof (char *));
    /* set are counter and test */
    currFile = fileList;

    /* open directory stream */
    dirp = opendir(path);
    if (dirp == NULL) 
    	{
	return((char **)NULL);
	}

    /* now loop */
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) 
	{
	/* parse out ./ and ../ respectively */
	if ( !strcmp(dp->d_name, ".") )
		continue;
	if ( !strcmp(dp->d_name, "..") )
		continue;
	*currFile++ = (char *)G_store(dp->d_name);
	}
    /* There should always be room for this since we're
     * parsing out . & ..
     */

    /* null terminate the list */
    *currFile = NULL; /* don't leave it pointing to hyperspace */
    /* alphabatize list */
    qsort(fileList, entryCnt-2, sizeof(char *), StrCmp);
    *num = entryCnt -2;
    return(fileList);
    }


/* debugging routine */
void PrintFileList(file_list)
char **file_list;
    {
    if (file_list == NULL)
	return;
    while(*file_list != NULL)
    puts(*file_list++);
    }

StrCmp(file1, file2)
    char **file1, **file2;
    {
    char *f1 = *file1;
    char *f2 = *file2;
    return(strcmp(f1, f2));
    }
