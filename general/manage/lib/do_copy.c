#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include "list.h"

static int recursive_copy(const char *src, const char *dst);

/*
 *  returns 0 - success
 *          1 - error
 */
int do_copy (int n, char *old, char *mapset, char *new)
{
    int i, ret, len;
    char path[1024], path2[1024];
    int result = 0;

    G_debug (3, "Copy %s", list[n].alias );
    
    G_message ("Copy <%s> to current mapset as <%s>",
	       G_fully_qualified_name (old, mapset), new);

    len = get_description_len(n);

    hold_signals(1);
    if ( G_strcasecmp (list[n].alias, "vect") == 0 ) {
	ret = Vect_copy ( old, mapset, new, stderr );
	if ( ret == -1 ) {
	    G_warning ("Cannot copy <%s> to current mapset as <%s>",
		       G_fully_qualified_name(old, mapset), new );
	    result = 1;
	}
    } else {
	for (i = 0; i < list[n].nelem; i++)
	{
	    G__make_mapset_element (list[n].element[i]);
	    G__file_name (path, list[n].element[i], old, mapset);
	    if (access (path, 0) != 0)
	    {
		G_remove (list[n].element[i], new);
		if (G_verbose() == G_verbose_max())
		    G_message (_("%s: missing"), list[n].desc[i]);
		
		continue;
	    }
	    G__file_name (path2, list[n].element[i], new, G_mapset());
	    if ( recursive_copy(path, path2) == 1 ) 
	    {
                result = 1;
            }
	    else
	    {
		if (G_verbose() == G_verbose_max())
		    G_message (_("%s: copied"), list[n].desc[i]);
	    }
	}
    }

/* special case: remove (yes, remove) the secondary color table, if it exists */
    if (G_strcasecmp (list[n].element[0], "cell") == 0)
    {
	char colr2[50];

	sprintf (colr2, "colr2/%s", G_mapset());
	G_remove (colr2, new);
    }
    hold_signals(0);

    return result;
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
 * if src is a file:
 * 	if dst does not exist:
 * 		copy src to dst				RULE 1
 * 	if dst is a file:
 * 		delete dst and copy src to dst		RULE 2
 * 	if dst is a directory:
 * 		try recursive_copy(src, dst/src)	RULE 3
 * if src is a directory:
 * 	if dst does not exist:
 * 		copy src to dst				RULE 1
 * 	if dst is a file:
 * 		delete dst and copy src to dst		RULE 2
 * 	if dst is a directory:
 * 		try					RULE 4
 * 		for i in `ls src`
 * 		do
 * 			recursive_copy(src/$i, dst/$i)
 * 		done
 *
 * RETURN: 0 if successful, otherwise 1
 */
static int
recursive_copy(const char *src, const char *dst)
{
	DIR *dirp;
	struct dirent *dp;
	struct stat sb;
	char buf[1024], buf2[1024];
	int fd, fd2;
	size_t len, len2;
	mode_t mode;

	if(G_lstat(src, &sb))
		return 1;

	/* src is a file */
	if(!S_ISDIR((mode = sb.st_mode)))
	{
		if(!G_lstat(dst, &sb) && S_ISDIR(sb.st_mode))
		{
			const char *p = strrchr(src, '/');
			/* src => dst/src */
			sprintf(buf, "%s/%s", dst, (p?p+1:src));

			return recursive_copy(src, buf);
		}

		/* src => dst */
		if((fd = open(src, O_RDONLY)) < 0)
			return 1;

		if((fd2 = open(dst, O_CREAT|O_TRUNC|O_WRONLY, mode & 0777)) < 0)
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

	/* src is a directory */

	if(G_lstat(dst, &sb))
	{
		if(G_mkdir(dst))
			return 1;
	}else
	/* if dst already exists and it's a file, try to remove it */
	if(!S_ISDIR(sb.st_mode))
	{
		if(remove(dst) || G_mkdir(dst))
			return 1;
	}

	if((dirp = opendir(src)) == NULL)
		return 1;
	while((dp = readdir(dirp)) != NULL)
	{
		/* do not copy hidden files */
		if(dp->d_name[0] == '.')
			continue;
		sprintf(buf, "%s/%s", src, dp->d_name);
		sprintf(buf2, "%s/%s", dst, dp->d_name);
		if(recursive_copy(buf, buf2))
			return 1;
	}
	closedir(dirp);

	return 0;
}
