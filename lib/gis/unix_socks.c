/*
****************************************************************************
*
* LIBRARY:      unix_socks.c  -- Routines related to using UNIX domain 
*               sockets for IPC mechanisms (such as XDRIVER).
*
* AUTHOR(S):    Eric G. Miller
*
* PURPOSE:      Historically GRASS has used FIFO for interprocess communic-
*               ations for display functions.  Unfortunately, FIFO's are
*               not available on all target platforms.  An attempt has been
*               made to use IPC message passing, but the semantics are
*               variable and it also isn't available on all target platforms.
*               UNIX sockets, or local or domain sockets, are much more
*               widely available and consistent.  NOTE: This implementation
*               of UNIX sockets provides zero security checking so should
*               not be used from untrusted clients.
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#ifndef __MINGW32__

#include "gis.h"
#include "version.h"
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>

/** For systems where the *_LOCAL (POSIX 1g) is not defined 
 ** There's not really any difference between PF and AF in practice.
 **/
#ifndef AF_LOCAL
#define AF_LOCAL AF_UNIX
#endif
#ifndef PF_LOCAL
#define PF_LOCAL PF_UNIX
#endif


/* ---------------------------------------------------------------------
 * _get_make_sock_path(), builds and tests the path for the socket
 * directory.  Returns NULL on any failure, otherwise it returns the
 * directory path. The path will be like 
 * "/tmp/grass6-$USER-$GIS_LOCK".
 * ($GIS_LOCK is set in lib/init/init.sh to PID) 
 * ---------------------------------------------------------------------*/
static char *
_get_make_sock_path (void)
{
    char *path, *user, *lock;
    const char *prefix = "/tmp/grass6";
    int len, status;
    struct stat theStat;
    
    user = G_whoami(); /* Don't free() return value ever! */
    if (user == NULL)
        return NULL;
    else if (user[0] == '?') /* why's it do that? */
    {
        return NULL;
    }

    if ( (lock = getenv ( "GIS_LOCK" )) == NULL )
	G_fatal_error ("Cannot get GIS_LOCK enviroment variable value");

    len = strlen(prefix) + strlen(user) + strlen(GRASS_VERSION_MAJOR) + strlen(GRASS_VERSION_MINOR) + strlen(lock) + 3;
    path = G_malloc (len);
    
    sprintf (path, "%s-%s-%s", prefix, user, lock);

    if ((status = lstat (path, &theStat)) != 0)
    {
        status = mkdir (path, S_IRWXU);
    }
    else 
    {
        if (!S_ISDIR (theStat.st_mode))
        {
            status = -1;  /* not a directory ?? */
        }
        else
        {
            status = chmod (path, S_IRWXU); /* fails if we don't own it */
        }
    }

    if (status) /* something's wrong if non-zero */
    {
        G_free (path);
        path = NULL;
    }

    return path;
}

        
 /* ----------------------------------------------------------------------
 * G_sock_get_fname(), builds the full path for a UNIX socket.  Caller 
 * should free() the return value when it is no longer needed.  Returns
 * NULL on failure.
 * ---------------------------------------------------------------------*/
char *
G_sock_get_fname (char *name)
{
    char *path, *dirpath;
    int len;

    if (name == NULL)
        return NULL;
    
    dirpath = _get_make_sock_path();
    
    if (dirpath == NULL)
        return NULL;

    len = strlen (dirpath) + strlen(name) + 2;
    path = G_malloc (len);
    sprintf (path, "%s/%s", dirpath, name);
    G_free (dirpath);

    return path;
}


/* -------------------------------------------------------------------
 * G_sock_exists(char *): Returns 1 if path is to a UNIX socket that
 * already exists, 0 otherwise.
 * -------------------------------------------------------------------*/
    
int
G_sock_exists (char *name)
{
    struct stat theStat;

    if (name == NULL || stat (name, &theStat) != 0)
        return 0;

    if (S_ISSOCK (theStat.st_mode))
        return 1;
    else
        return 0;
}


/* -----------------------------------------------------------------
 * G_sock_bind (char *): Takes the full pathname for a UNIX socket
 * and returns the file descriptor to the socket after a successful
 * call to bind().  On error, it returns -1.  Check "errno" if you
 * want to find out why this failed (clear it before the call).
 * ----------------------------------------------------------------*/

int
G_sock_bind (char *name)
{
    int    sockfd;
    size_t size;
    struct sockaddr_un addr;

    if (name == NULL)
        return -1;

    /* Bind requires that the file does not exist. Force the caller
     * to make sure the socket is not in use.  The only way to test,
     * is a call to connect().
     */
    if (G_sock_exists (name))
    {
        errno = EADDRINUSE;
        return -1;
    }

    /* must always zero socket structure */
    memset (&addr, 0, sizeof(addr));

    /* The path to the unix socket must fit in sun_path[] */
    if (sizeof (addr.sun_path) < strlen(name) + 1)
        return -1;
    
    strncpy (addr.sun_path, name, sizeof (addr.sun_path) - 1);
    
    addr.sun_family = AF_LOCAL;

    sockfd = socket (PF_LOCAL, SOCK_STREAM, 0);

    size = (offsetof (struct sockaddr_un, sun_path) 
            + strlen (addr.sun_path) + 1);

    if (bind (sockfd, (struct sockaddr *) &addr, size) != 0)
        return -1;

    return sockfd;
}


/* ---------------------------------------------------------------------
 * G_sock_listen(int, unsigned int): Wrapper around the listen() 
 * function.
 * --------------------------------------------------------------------*/

int
G_sock_listen (int sockfd, unsigned int queue_len)
{
    return listen (sockfd, queue_len);
}

/* -----------------------------------------------------------------------
 * G_sock_accept (int sockfd):
 * Wrapper around the accept() function. No client info is returned, but
 * that's not generally useful for local sockets anyway.  Function returns
 * the file descriptor or an error code generated by accept().  Note,
 * this call will usually block until a connection arrives.  You can use
 * select() for a time out on the call.
 * ---------------------------------------------------------------------*/

int
G_sock_accept (int sockfd)
{
    struct sockaddr_un addr;
    int len = sizeof(addr);
    return accept (sockfd, (struct sockaddr *) &addr, &len);
}
 

/* ----------------------------------------------------------------------
 * G_sock_connect (char *name):  Tries to connect to the unix socket
 * specified by "name".  Returns the file descriptor if successful, or
 * -1 if unsuccessful.  Global errno is set by connect() if return is -1
 * (though you should zero errno first, since this function doesn't set
 * it for a couple conditions).
 * --------------------------------------------------------------------*/

int
G_sock_connect (char *name)
{
    int    sockfd;
    struct sockaddr_un addr;

    if (!G_sock_exists (name))
        return -1;

    /* must always zero socket structure */
    memset (&addr, 0, sizeof(addr));

    /* The path to the unix socket must fit in sun_path[] */
    if (sizeof (addr.sun_path) < strlen(name) + 1)
        return -1;
    
    strncpy (addr.sun_path, name, sizeof (addr.sun_path) - 1);
    
    addr.sun_family = AF_LOCAL;

    sockfd = socket (PF_LOCAL, SOCK_STREAM, 0);

    if (connect (sockfd, (struct sockaddr *) &addr, sizeof (addr)) != 0)
        return -1;
    else
        return sockfd;
}

/* vim: set softtabstop=4 shiftwidth=4 expandtab : */

int
G_sock_socketpair(int family, int type, int protocol, int *fd)
{
	int		n;

	if ( (n = socketpair(family, type, protocol, fd)) < 0)
		return -1;
	else 
		return 0;
}

#endif
