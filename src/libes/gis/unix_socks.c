/*
*
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
 * directory path. The path will be like "/tmp/grass-$USER".
 * ---------------------------------------------------------------------*/
static char *
_get_make_sock_path (void)
{
    char *path, *user;
    const char *prefix = "/tmp/grass-";
    int len, status;
    struct stat theStat;
    
    user = G_whoami(); /* Don't free() return value ever! */
    if (user == NULL)
        return NULL;
    else if (user[0] == '?') /* why's it do that? */
    {
        return NULL;
    }

    len = strlen(prefix) + strlen(user) + 1;
    path = G_malloc (len);
    sprintf (path, "%s%s", prefix, user);

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

/*!
 * \brief makes full socket path
 *
 * Takes a simple <b>name</b> for a communication channel and builds the
 * full path for a sockets file with that <b>name</b>.  The path as of
 * this writing (2000-02-18) is located in the temporary directory for the
 * user's current mapset (although this will likely change).  A <b>NULL</b>
 * pointer is returned if the function fails for some reason.  The caller is
 * responsible for freeing the memory of the returned string when it is no
 * longer needed.
 *
 *  \param name
 *  \return char * 
 */

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
    

/*!
 * \brief does the socket exist
 *
 * Takes the full path to a unix socket; determines if the file exists; and if
 * the file exists whether it is a socket file or not.  Returns a non-zero
 * value if the file exists and is a socket file.  Otherwise it returns
 * zero.
 *
 *  \param name
 *  \return int
 */

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


/*!
 * \brief binds the socket
 *
 * Takes the full path to a unix socket and attempts to bind a file
 * descriptor to the path <b>name</b>.  If successful it will return
 * the file descriptor.  Otherwise, it returns -1.  The socket file
 * must not already exist.  If it does, this function will fail and
 * set the global <b>errno</b> to <b>EADDRINUSE</b>.  Other error
 * numbers may be set if the call to <b>bind()</b> fails.  Server
 * programs wishing to bind a socket should test if the socket file
 * they wish to use already exists.  And, if so, they may try to 
 * connect to the socket to see if it is in use.  If it is not in use,
 * such programs may then call <b>unlink()</b> or <b>remove()</b>
 * to delete the file before calling <b>G_sock_bind()</b>.  It is
 * important that server processes do not just delete existing socket
 * files without testing the connection.  Doing so may make another
 * server process unreachable (i.e. you will have hijacked the other
 * server's communication channel).  Server processes must call
 * <b>G_sock_bind()</b> prior to calling <b>G_sock_listen()</b>
 * and <b>G_sock_accept()</b>.
 *
 *  \param name
 *  \return int
 */

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


/*!
 * \brief listen on a socket
 *
 * Takes the file descriptor returned by a successful call to
 * <b>G_sock_bind()</b> and the length of the the listen queue.
 * A successful call will return 0, while a failed call will return -1.
 * The global <b>errno</b> will contain the error number corresponding
 * to the reason for the failure.  The queue length should never be zero.
 * Some systems may interpret this to mean that no connections should be
 * queued.  Other systems may add a fudge factor to the queue length that
 * the caller specifies.  Servers that don't want additional connections
 * queued should <b>close()</b> the listening file descriptor after
 * a successful call to <b>G_sock_accept()</b>.  This function is
 * a simple wrapper around the system <b>listen()</b> function.
 *
 *  \param fd
 *  \param queue
 *  \return int
 */

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


/*!
 * \brief accept a connection on the listening socket
 *
 * Takes the file descriptor returned by a successful call to
 * <b>G_sock_bind()</b>, for which a successful call to
 * <b>G_sock_listen()</b> has also been made, and waits for an incoming
 * connection.  When a connection arrives, the file descriptor for the connection
 * is returned.  This function normally blocks indefinitely.  However, an
 * interrupt like <b>SIGINT</b> may cause this function to return without a
 * valid connection.  In this case, the return value will be -1 and the global
 * error number will be set to <b>EINTR</b>.  Servers should handle this
 * possibility by calling <b>G_sock_accept()</b> again.  A typical server
 * might have a call to <b>fork()</b> after a successful return from
 * <b>G_sock_accept()</b>.  A server might also use <b>select()</b> to see
 * if an a connection is ready prior to calling <b>G_sock_accept()</b>. This
 * function is a simple wrapper around the system's <b>accept()</b> function,
 * with the second and third arguments being <b>NULL</b>.
 *
 *  \param fd
 *  \return int
 */

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


/*!
 * \brief make a connection to a server process
 *
 * Takes the full path to a socket file and attempts
 * to make a connection to a server listening for connections.  If successful,
 * the file descriptor for the socket connection is returned.  Otherwise, -1
 * is returned and the global <b>errno</b> may be set.  This function and
 * <b>G_sock_get_fname()</b> are the only functions a client program
 * really needs to worry about.  If the caller wants to be sure that the
 * global error number was set from an unsuccessful call to this function,
 * she should zero <b>errno</b> prior to the call.  Failures due to
 * a non-existent socket file or a path name that exceeds system limits,
 * will not change the global error number.
 *
 *  \param name
 *  \return int
 */

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
#endif
