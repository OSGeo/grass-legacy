static char     rcsid[] = "@(#)XGRASS $Id: dirlist.c,v 0.0 1992/05/05 14:56:15 sink Exp sink $";
/*
 * File: dirlist.c
 * 
 * Desc: contains code for getting a directory listing and to free associated
 * memory
 * 
 * Auth: Kurt Buehler
 * 
 * Date: Tue Nov  5 16:21:47 CST 1991
 * 
 * Modification History:
 * 
 * 
 */

#include "xgrass_lib.h"

#ifdef _NO_PROTO
int
_XgDirectoryNotEmpty(path, dotFiles, dirsonly)
    char           *path;       /* the path for the directory we want listed */
    Boolean         dotFiles;
    int             dirsonly;   /* true if we should only list the
                                 * subdirectories */
#else
int
_XgDirectoryNotEmpty(char *path, Boolean dotFiles, int dirsonly)
#endif
{
    char            subpath[512];
    DIR            *dirp;
    DIR            *subdirp;
    DIR            *opendir();
#if defined(sparc)||defined(IGRAPH)||defined(uts)||defined(SVR4)||defined(AIX)
    struct dirent  *dp;
    struct dirent  *readdir();
#else
    struct direct  *dp;
    struct direct  *readdir();
#endif
    int             i = 0, numItems = 0;

    if (path == NULL)
        return NULL;

    dirp = opendir(path);
    if (!dirp)
        return NULL;
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
        if (dirsonly) {
            sprintf(subpath, "%s/%s", path, dp->d_name);
            if ((subdirp = opendir(subpath)) != NULL) {
                closedir(subdirp);
                if (*(dp->d_name) == '.') {
                    if (dotFiles &&
                      strcmp(dp->d_name, ".") && strcmp(dp->d_name, "..")) {
                        closedir(dirp);
                        return 1;
                    }
                } else {
                    closedir(dirp);
                    return 1;
                }
            }
        } else {
            if (*(dp->d_name) == '.') {
                if (dotFiles &&
                    strcmp(dp->d_name, ".") && strcmp(dp->d_name, "..")) {
                    closedir(dirp);
                    return 1;
                }
            } else {
                closedir(dirp);
                return 1;
            }
        }
    }
    closedir(dirp);
    return 0;
}

#ifdef _NO_PROTO
char          **
_XgDirectoryListing(path, items, dotFiles, sort, dirsonly)
    char           *path;       /* the path for the directory we want listed */
    int            *items;      /* number of items returned */
    Boolean         dotFiles;
    int             sort;       /* 1 is ascending 2 is descending 0 is no
                                 * sort */
    int             dirsonly;   /* true if we should only list the
                                 * subdirectories */
#else
char          **
_XgDirectoryListing(char *path, int *items, Boolean dotFiles, int sort, int dirsonly)
#endif
{
    char          **list;
    char            subpath[512];
    DIR            *dirp;
    DIR            *subdirp;
    DIR            *opendir();
#if defined (IGRAPH)||defined(sparc)||defined(uts)||defined(SVR4)||defined(AIX)
    struct dirent  *dp;
    struct dirent  *readdir();
#else
    struct direct  *dp;
    struct direct  *readdir();
#endif
    int             i = 0, numItems = 0;

    *items = 0;

    if (path == NULL)
        return NULL;

    /* Count the directory items so we know how much mem to allocate */
    dirp = opendir(path);
    if (!dirp)
        return NULL;
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
        if (dirsonly) {
            sprintf(subpath, "%s/%s", path, dp->d_name);
            if ((subdirp = opendir(subpath)) != NULL) {
                closedir(subdirp);
                if (*(dp->d_name) == '.') {
                    if (dotFiles &&
                        strcmp(dp->d_name, ".") && strcmp(dp->d_name, ".."))
                        numItems++;
                } else
                    numItems++;
            }
        } else {
            if (*(dp->d_name) == '.') {
                if (dotFiles &&
                    strcmp(dp->d_name, ".") && strcmp(dp->d_name, ".."))
                    numItems++;
            } else
                numItems++;
        }
    }
    closedir(dirp);

    /* Allocate mem and fetch directory listing */
    *items = numItems;
    list = (char **) _XgCalloc(numItems, sizeof(char *));

    dirp = opendir(path);
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
        if (dirsonly) {
            sprintf(subpath, "%s/%s", path, dp->d_name);
            if ((subdirp = opendir(subpath)) != NULL) {
                closedir(subdirp);
                if (*(dp->d_name) == '.') {
                    if (dotFiles &&
                        strcmp(dp->d_name, ".") && strcmp(dp->d_name, ".."))
                        list[i++] = _XgStrDup(dp->d_name);
                } else
                    list[i++] = _XgStrDup(dp->d_name);
            }
        } else {
            if (*(dp->d_name) == '.') {
                if (dotFiles &&
                    strcmp(dp->d_name, ".") && strcmp(dp->d_name, ".."))
                    list[i++] = _XgStrDup(dp->d_name);
            } else
                list[i++] = _XgStrDup(dp->d_name);
        }
    }
    closedir(dirp);

    if (sort)
        _XgSortStringArray(list, numItems, sort);

    return list;
}

#ifdef _NO_PROTO
void
_XgFreeDirectoryListing(ptr, items)
    char          **ptr;
    int             items;
#else
void
_XgFreeDirectoryListing(char **ptr, int items)
#endif
{
    _XgFreeStringArray(ptr, items);
}

int
#ifdef _NO_PROTO
_XgPutDirectoryInListOWN(path, list, dirsonly)
    char           *path;
    Widget          list;
    int             dirsonly;
#else
_XgPutDirectoryInListOWN(char *path, Widget list, int dirsonly)
#endif
{
    char          **DirList;
    int             DirCount;
    XmStringTable   str_list;
    int             i;
    int             j;

    XmListDeselectAllItems(list);

    DirList = _XgDirectoryListing(path, &DirCount, 0, 1, dirsonly);
    str_list = (XmStringTable) XtMalloc(DirCount * sizeof(XmString *));

    j = 0;
    for (i = 0; i < DirCount; i++) {
        if (!(G__mapset_permissions(DirList[i]) != 1)) {
            str_list[j++] = XmStringCreateLtoR(DirList[i], XmSTRING_DEFAULT_CHARSET);
        }
    }

    XtVaSetValues(list,
                  XmNitems, str_list,
                  XmNitemCount, j,
                  NULL);

    if (DirCount)
        _XgFreeDirectoryListing(DirList, DirCount);

    for (i = 0; i < j; i++)
        XmStringFree(str_list[i]);
    XtFree(str_list);
    return j;
}

char **
#ifdef _NO_PROTO
_XgSearchPath(cp)
int *cp;
#else
_XgSearchPath(int *cp)
#endif
{
    char **array;
    int             i;
    int             count;
    char           *name;

    G_reset_mapsets();
    for (count = 0; name = (char *)G__mapset_name(count); count++) {
    }

    array = (char **)_XgCalloc(count+1,sizeof(char *));

    for (i = 0; i < count; i++)
	array[i] = _XgStrDup((char *)G__mapset_name(i));

    array[count] = NULL;

    *cp = count;
    return array;
}

void
#ifdef _NO_PROTO
_XgPutSearchPathInList(list)
    Widget          list;
#else
_XgPutSearchPathInList(Widget list)
#endif
{
    XmStringTable   str_list;
    int             i;
    int             count;
    char           *name;

    XmListDeselectAllItems(list);

    for (count = 0; name = (char *)G__mapset_name(count); count++) {
    }



    str_list = (XmStringTable) XtMalloc(count * sizeof(XmString *));

    for (i = 0; i < count; i++)
        str_list[i] = XmStringCreateLtoR((char *)G__mapset_name(i), XmSTRING_DEFAULT_CHARSET);

    XtVaSetValues(list,
                  XmNitems, str_list,
                  XmNitemCount, count,
                  NULL);

    for (i = 0; i < count; i++)
        XmStringFree(str_list[i]);
    XtFree(str_list);
}

void
#ifdef _NO_PROTO
_XgPutDirectoryInList(path, list, dirsonly)
    char           *path;
    Widget          list;
    int             dirsonly;
#else
_XgPutDirectoryInList(char *path, Widget list, int dirsonly)
#endif
{
    char          **DirList;
    int             DirCount;
    XmStringTable   str_list;
    int             i;

    XmListDeselectAllItems(list);

    DirList = _XgDirectoryListing(path, &DirCount, 0, 1, dirsonly);
    str_list = (XmStringTable) XtMalloc(DirCount * sizeof(XmString *));

    for (i = 0; i < DirCount; i++)
        str_list[i] = XmStringCreateLtoR(DirList[i], XmSTRING_DEFAULT_CHARSET);

    XtVaSetValues(list,
                  XmNitems, str_list,
                  XmNitemCount, DirCount,
                  NULL);

    if (DirCount)
        _XgFreeDirectoryListing(DirList, DirCount);

    for (i = 0; i < DirCount; i++)
        XmStringFree(str_list[i]);
    XtFree(str_list);
}
