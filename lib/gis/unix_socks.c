/**
 * \file unix_sockets.c
 *
 * \brief Unix sockets support functions.
 *
 * Routines related to using UNIX domain sockets for IPC mechanisms 
 * (such as XDRIVER).<br>
 *
 * Historically GRASS has used FIFO for interprocess communications for 
 * display functions. Unfortunately, FIFO's are not available on all 
 * target platforms. An attempt has been made to use IPC message 
 * passing, but the semantics are variable and it also isn't available 
 * on all target platforms. UNIX sockets, or local or domain sockets, 
 * are much more widely available and consistent.<br>
 *
 * <b>Note:</b> This implementation of UNIX sockets provides zero 
 * security checking so should not be used from untrusted clients.<br>
 *
 * This program is free software under the GNU General Public License
 * (>=v2). Read the file COPYING that comes with GRASS for details.
 *
 * \author Eric G. Miller
 *
 * \date 1999-2006
 */

#ifndef __MINGW32__

#include <grass/gis.h>
#include <grass/version.h>
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


static char *_get_make_sock_path (void);


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
    
    user = G_whoami(); /* Don't G_free () return value ever! */
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

    if ((status = G_lstat (path, &theStat)) != 0)
    {
        status = G_mkdir (path);
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

        
/**
 * \fn char *G_sock_get_fname (const char *name)
 *
 * \brief Builds full path for a UNIX socket.
 *
 * Caller should <i>G_free()</i> the return value when it is no longer 
 * needed.
 *
 * \param[in] name
 * \return NULL on error
 * \return Pointer to string socket path on success
 */

char *
G_sock_get_fname (const char *name)
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


/**
 * \fn int G_sock_exists (const char *name)
 *
 * \brief Checks socket existence.
 *
 * \param[in] name
 * \return 1 if <b>name</b> exists
 * \return 0 if <b>name</b> does not exist
 */

int
G_sock_exists (const char *name)
{
    struct stat theStat;

    if (name == NULL || stat (name, &theStat) != 0)
        return 0;

    if (S_ISSOCK (theStat.st_mode))
        return 1;
    else
        return 0;
}


/**
 * \fn int G_sock_bind (const char *name)
 *
 * \brief Binds socket to file descriptor.
 *
 * Takes the full pathname for a UNIX socket and returns the file 
 * descriptor to the socket after a successful call to <i>bind()</i>.
 *
 * \param[in] name
 * \return -1 and "errno" is set on error
 * \return file descriptor on success
 */

int
G_sock_bind (const char *name)
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


/**
 * \fn int G_sock_listen (int sockfd, unsigned int queue_len)
 *
 * \brief Wrapper function to <i>listen()</i>.
 *
 * \param[in] sockfd
 * \param[in] queue_len
 * \return 0 on success
 * \return -1 and "errno" set on error
 */

int
G_sock_listen (int sockfd, unsigned int queue_len)
{
    return listen (sockfd, queue_len);
}


/**
 * \fn int G_sock_accept (int sockfd)
 *
 * \brief Wrapper around <i>accept()</i>.
 *
 * <b>Note:</b> This call will usually block until a connection arrives. 
 * <i>select()</i> can be used for a time out on the call.
 *
 * \param[in] sockfd
 * \return -1 and "errno" set on error
 * \return file descriptor on success
 */

int
G_sock_accept (int sockfd)
{
    struct sockaddr_un addr;
    int len = sizeof(addr);
    return accept (sockfd, (struct sockaddr *) &addr, &len);
}
 

/**
 * \fn int G_sock_connect (const char *name)
 *
 * \brief Tries to connect to the UNIX socket specified by <b>name</b>.
 *
 * \param[in] name
 * \return -1 and "errno" set on error
 * \return file descriptor on success
 */

int
G_sock_connect (const char *name)
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


/**
 * \fn int G_sock_socketpair (int family, int type, int protocol, int *fd)
 *
 * \brief Creates an unnamed pair of connected sockets.
 *
 * \param[in] family
 * \param[in] protocol
 * \param[in] fd
 * \return -1 and "errno" set on error
 * \return 0 on success
 */

int
G_sock_socketpair(int family, int type, int protocol, int *fd)
{
	int		n;

	if ( (n = socketpair(family, type, protocol, fd)) < 0)
		return -1;
	else 
		return 0;
}

/* vim: set softtabstop=4 shiftwidth=4 expandtab : */
#endif
