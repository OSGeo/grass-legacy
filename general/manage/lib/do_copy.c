#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <dirent.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include "list.h"

#ifdef __MINGW32__
#define mkdir(name, mode) mkdir(name)
#define lstat(path, sb) stat(path, sb)
#endif

static int recursive_copy(const char *from, const char *to);

int do_copy (int n, char *old, char *mapset, char *new)
{
    int i, ret, len;
    char path[1024], path2[1024];

    G_debug (3, "Copy %s", list[n].alias );
    fprintf (stdout,"COPY [%s] to current mapset as [%s]\n", G_fully_qualified_name(old, mapset), new);

    len = get_description_len(n);

    hold_signals(1);
    if ( strcmp(list[n].alias, "vect") == 0 ) {
	ret = Vect_copy ( old, mapset, new, stderr );
	if ( ret == -1 ) {
	    G_warning ("Cannot copy %s to current mapset as %s", G_fully_qualified_name(old, mapset), new );
	}
    } else {
	for (i = 0; i < list[n].nelem; i++)
	{
	    fprintf (stdout," %-*s ", len, list[n].desc[i]);
	    fflush (stdout);


	    G__make_mapset_element (list[n].element[i]);
	    G__file_name (path, list[n].element[i], old, mapset);
	    if (access (path, 0) != 0)
	    {
		G_remove (list[n].element[i], new);
		fprintf (stdout,"MISSING\n");
		continue;
	    }
	    G__file_name (path2, list[n].element[i], new, G_mapset());
	    recursive_copy(path, path2);
	    fprintf (stdout,"\n");
	}
    }

/* special case: remove (yes, remove) the secondary color table, if it exists */
    if (strcmp (list[n].element[0], "cell") == 0)
    {
	char colr2[50];

	sprintf (colr2, "colr2/%s", G_mapset());
	G_remove (colr2, new);
    }
    hold_signals(0);

    return 0;
}

/* RULE:
 * 1. If destination does not exist, copy source to destination as expected.
 * 2. If destination already exists and it's a file, destination will be
 *    deleted first and apply RULE 1.
 * 3. If destination already exists which is a directory and source is a file,
 *    try to copy source to destination directory.
 * 4. If destination already exists which is a directory and source is also a
 *    directory, try to copy all contents in source to destination directory.
 *
 * This rule is designed according to general/manage/lib/copy.sh.
 *
 * POSSIBLE CASES:
 * if 'from' is a file:
 * 	if 'to' does not exist:
 * 		copy 'from' to 'to'			RULE 1
 * 	if 'to' is a file:
 * 		delete 'to' and copy 'from' to 'to'	RULE 2
 * 	if 'to' is a directory:
 * 		try recursive_copy('from', 'to/from')	RULE 3
 * if 'from' is a directory:
 * 	if 'to' does not exist:
 * 		copy 'from' to 'to'			RULE 1
 * 	if 'to' is a file:
 * 		delete 'to' and copy 'from' to 'to'	RULE 2
 * 	if 'to' is a directory:
 * 		try					RULE 4
 * 		for i in `ls from`
 * 		do
 * 			recursive_copy('from/$i', 'to/$i')
 * 		done
 */
static int
recursive_copy(const char *from, const char *to)
{
	DIR *dirp;
	struct dirent *dp;
	struct stat sb;
	char buf[1024], buf2[1024];
	int fd, fd2;
	size_t len, len2;
	mode_t mode;

	if(lstat(from, &sb))
		return 1;

	/* from is a file */
	if(!S_ISDIR((mode = sb.st_mode)))
	{
		if(!lstat(to, &sb) && S_ISDIR(sb.st_mode))
		{
			const char *p = strrchr(from, '/');
			/* from => to/from */
			sprintf(buf, "%s/%s", to, (p?p+1:from));

			return recursive_copy(from, buf);
		}

		/* from => to */
		if((fd = open(from, O_RDONLY)) < 0)
			return 1;

		if((fd2 = open(to, O_CREAT|O_TRUNC|O_WRONLY, mode & 0777)) < 0)
		{
			close(fd);
			return 1;
		}
		while((len = read(fd, buf, 1024)) > 0)
		{
			while(len && (len2 = write(fd2, buf, len)) >= 0)
				len -= len2;
		}
		close(fd);
		close(fd2);

		return 0;
	}

	/* from is a directory */

	if(lstat(to, &sb))
	{
		if(mkdir(to, mode & 0777))
			return 1;
	}else
	/* if to already exists and it's a file, try to remove it */
	if(!S_ISDIR(sb.st_mode))
	{
		if(remove(to) || mkdir(to, mode & 0777))
			return 1;
	}

	if((dirp = opendir(from)) == NULL)
		return 1;
	while((dp = readdir(dirp)) != NULL)
	{
		/* do not copy hidden files */
		if(dp->d_name[0] == '.')
			continue;
		sprintf(buf, "%s/%s", from, dp->d_name);
		sprintf(buf2, "%s/%s", to, dp->d_name);
		if(recursive_copy(buf, buf2))
			return 1;
	}
	closedir(dirp);

	return 0;
}
