#include "xgrass_lib.h"

#ifndef MCCABE
#include <pwd.h>
#endif

#ifdef USE_GETWD
#include <sys/param.h>
#define MAX_DIR_PATH_LEN    MAXPATHLEN
#else
#define MAX_DIR_PATH_LEN    1024
#endif
#define MAX_USER_NAME_LEN   256


char                           *
#ifdef _NO_PROTO
XgGetQualifiedDir(dirSpec)
    char                           *dirSpec;
#else
XgGetQualifiedDir(
                  char *dirSpec)
#endif
/****************
 * Builds directory name showing descriptive path components.  The result
 *   is a directory path beginning at the root directory and terminated
 *   with a '/'.  The path will not contain ".", "..", or "~" components.
 * The routine allocates memory for the result, which is guaranteed to be
 *   of length >= 1.  This memory should eventually be freed using XtFree().
 ****************/
{
    int                             dirSpecLen;
    struct passwd                  *userDir;
    int                             userDirLen;
    int                             userNameLen;
    char                           *outputBuf;
    char                           *destPtr;
    char                           *srcPtr;
    char                           *scanPtr;
    char                            nameBuf[MAX_USER_NAME_LEN];
    char                            dirbuf[MAX_DIR_PATH_LEN];

#ifdef USE_GETWD
    extern char                    *getwd();
#else
    extern char                    *getcwd();
#endif
    /****************/

    dirSpecLen = strlen(dirSpec);
    outputBuf = NULL;

    switch (*dirSpec) {
    case '~':
        {
            if (!(dirSpec[1]) || (dirSpec[1] == '/')) {
                userDir = (struct passwd *) getpwuid(getuid());
                if (userDir) {
                    userDirLen = strlen(userDir->pw_dir);
                    outputBuf = _XgMalloc(userDirLen + dirSpecLen + 2);
                    strcpy(outputBuf, userDir->pw_dir);
                    strcpy(&outputBuf[userDirLen], (dirSpec + 1));
                }
            } else {
                destPtr = nameBuf;
                userNameLen = 0;
                srcPtr = dirSpec + 1;
                while (*srcPtr && (*srcPtr != '/')
                       && (++userNameLen < MAX_USER_NAME_LEN)) {
                    *destPtr++ = *srcPtr++;
                }
                *destPtr = NULL;

                userDir = (struct passwd *) getpwnam(nameBuf);
                if (userDir) {
                    userDirLen = strlen(userDir->pw_dir);
                    dirSpecLen = strlen(srcPtr);
                    outputBuf = _XgMalloc(userDirLen + dirSpecLen + 2);
                    strcpy(outputBuf, userDir->pw_dir);
                    strcpy(&outputBuf[userDirLen], srcPtr);
                }
            }
            break;
        }
    case '/':
        {
            outputBuf = _XgMalloc(dirSpecLen + 2);
            strcpy(outputBuf, dirSpec);
            break;
        }
    default:
        {
#ifdef USE_GETWD
            destPtr = getwd(dirbuf);
#else
            destPtr = getcwd(dirbuf, MAX_DIR_PATH_LEN);
#endif
            if (destPtr) {
                userDirLen = strlen(destPtr);
                outputBuf = _XgMalloc(userDirLen + dirSpecLen + 3);
                strcpy(outputBuf, destPtr);
                outputBuf[userDirLen++] = '/';
                strcpy(&outputBuf[userDirLen], dirSpec);
            }
            break;
        }
    }
    if (!outputBuf) {
        outputBuf = _XgMalloc(2);
        outputBuf[0] = '/';
        outputBuf[1] = '\0';
    } else {
        userDirLen = strlen(outputBuf);
        if (outputBuf[userDirLen - 1] != '/') {
            outputBuf[userDirLen] = '/';
            outputBuf[++userDirLen] = NULL;
        }
        /*
         * The string in outputBuf is assumed to begin and end with a '/'.
         */
        scanPtr = outputBuf;
        while (*++scanPtr) {    /* Skip past '/'. *//* scanPtr now points to
                                 * non-NULL character following '/'. */
            if (scanPtr[0] == '.') {
                if (scanPtr[1] == '/') {        /* Have "./", so just erase
                                                 * (overwrite with shift). */
                    destPtr = scanPtr;
                    srcPtr = &scanPtr[2];
                    while (*destPtr++ = *srcPtr++) {
                    }
                    --scanPtr;  /* Leave scanPtr at preceding '/'. */
                    continue;
                } else {
                    if ((scanPtr[1] == '.') && (scanPtr[2] == '/')) {   /* Have "../", so back
                                                                         * up one directory. */
                        srcPtr = &scanPtr[2];
                        --scanPtr;      /* Move scanPtr to preceding '/'. */
                        if (scanPtr != outputBuf) {
                            while ((*--scanPtr != '/')) {
                            }   /* Now move to previous '/'. */
                        }
                        destPtr = scanPtr;
                        while (*++destPtr = *++srcPtr) {
                        }       /* Overwrite "../" with shift. */
                        continue;
                    }
                }
            } else {            /* Check for embedded "//".  Posix allows a
                                 * leading double slash (and Apollos require
                                 * it). */
                if (*scanPtr == '/') {
                    if ((scanPtr > (outputBuf + 1))
                        || (scanPtr[1] == '/')) {
                        /*
                         * Have embedded "//" (other than root
                         * specification), so erase with shift and reset
                         * scanPtr.
                         */
                        srcPtr = scanPtr;
                        --scanPtr;
                        destPtr = scanPtr;
                        while (*++destPtr = *++srcPtr) {
                        }
                    }
                    continue;
                }
            }
            while (*++scanPtr != '/') {
            }
        }
    }
    return (outputBuf);
}

void
#ifdef _NO_PROTO
XgStripTrailingSlash(s)
    char                           *s;
#else
XgStripTrailingSlash(char *s)
#endif
{
    int                             len;

    if (!s)
        return;
    len = strlen(s);
    if (s[len-1] == '/')
        if ( len == 1 ) return;
        else s[len-1] = '\0';
    return;
}
