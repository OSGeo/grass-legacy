static char rcsid[] = "@(#)XGRASS $Id: relative.c,v 0.0 1992/05/05 14:56:21 sink Exp sink $";
/*
 * File: relative.c
 *
 * Desc: calculates relative path from start (must be a directory) to path.
 *       NOTE: BOTH start and path must be fully qualified paths.
 *       Users are responsible for free'ing the resulting string.
 *
 * Auth: Kurt Buehler
 *
 * Date: Wed Dec  4 08:14:25 CST 1991
 *
 * Modification History:
 *
 *
 */

#include "xgrass_lib.h"

#ifdef _NO_PROTO
char                           *
XgRelativePath(start, path)
    char                           *start;
    char                           *path;
#else
char                           *
XgRelativePath(char *start, char *path)
#endif
{
    char                           *result, *rest;
    int                             i, j, slashes = 0;
    int                             slen, plen;
    int                             length;
    int                             firstdiff = -1;

    if (!start || !path) {
        /* can't be NULL */
        return NULL;
    } else if (start[0] != '/' || path[0] != '/') {
        /* must be fully qualified paths (starting from root */
        return NULL;
    }
    slen = strlen(start);
    plen = strlen(path);
    /* if start is the root, return ".path" */
    if (!strncmp(start, "/", slen)) {
        result = _XgMalloc(plen + 1);
        bzero(result, plen + 1);
        strcat(result, ".");
        strcat(result, path);
        return result;
        /* if both are the same return "./" */
    } else if (slen == plen && !strncmp(start, path, slen)) {
        result = _XgMalloc(3);
        bzero(result, 3);
        strcat(result, "./");
        return result;
    }
    length = ((slen > plen) ? slen : plen);

    for (i = 0; i < length; i++) {
        if (strncmp(start, path, i)) {
            firstdiff = i;
            break;
        }
    }
    for (i = firstdiff; path[i] != '/'; i--);
    i++;
    rest = path + i;
    for (j = i - 1; j < slen; j++) {
        if (start[j] == '/') {
            slashes++;
        }
    }
    result = _XgMalloc(2 + (slashes * 3) + strlen(rest) + 1);
    bzero(result, (2 + slashes * 3) + strlen(rest) + 1);
    strcat(result, "./");
    for (j = 0; j < slashes; j++) {
        strcat(result, "../");
    }
    strcat(result, rest);
    return result;
}
