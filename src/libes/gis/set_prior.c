/*  @(#)set_prior.c    2.1  6/26/87  */
/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

/*
**  	For this file, and this file only I have set it up to act on
**      special compiler defines taken from config.h.
**
**	    Ideally we are trying to set it up so that priority is set
**	only when needed, (i.e. when collecting points in stream mode.)
**	To do this Collect_points calls set_priority()/unset_priority()
**	before and after talking w/ the digitizer.
**
**      If you have defined MASSCOMP in the DIGIT_OPTS in makehead,
**	digit will not run as root.  On Masscomps anyone can issue a
**	nice () request  so there is no need to run set-user as root.
**
**	Written by Dave Gerdes  4/1988
**
*/

#include "config.h"

#include <stdio.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*
*  set_priority() - this functions sets the raises the priority of the program.
*    This is done to make sure no points are lost when digitizing in STREAM
*    mode.   This means that this program will get more CPU time then any
*    other program.  It will also take CPU time from graphics and ethernet.
*    If graphics or ethernet seem to be degraded, the priority may have
*    to be lessened.
*    In other words the priority value may differ from machine to machine.
*/
static int swap_re_uids (void);

#ifndef
  #define    PRIO_PROCESS    0
#endif

/*  WARNING  -18 gets the most CPU time, priority > -18 gets less CPU time  */
/*  If the graphics don't come out except after digitizing a line then your
*   running too fast and the CPU isn't giving the graphics monitor a chance
*   to update. -mh
*/

#define    NORMAL        0
#define    PRIORITY    -10

/*  read warning above  */

static  int  priority_set = 0;

int init_priority (void)
{

#ifdef MASSCOMP
/* MASSCOMP does not require SU to set priorities so just turn off SU */
    if (getuid() == 0 || geteuid() == 0)
	if (getuid() != 0)
	    setuid (getuid());
	else
	    setuid (geteuid ());
#endif
    return 0;
}

/*  set_priority() returns 1 is already set and 0 if it had to set it.
*/

int set_priority ()
{
    if (priority_set)
    	return(priority_set);

    swap_re_uids ();	/* set to root */

#ifdef HAVE_SETPRIORITY
    setpriority (PRIO_PROCESS, (int) getpid (), PRIORITY);
#else
#ifdef HAVE_NICE
    nice (PRIORITY);
#endif
#endif /* HAVE_SETPRIORITY */

    swap_re_uids ();	/* and back to user */

    priority_set = 1;
    return(0);
}

int unset_priority ()
{
    swap_re_uids ();	/* set to root */

#ifdef HAVE_SETPRIORITY
    setpriority (PRIO_PROCESS, (int) getpid (), NORMAL);
#else
#ifdef HAVE_NICE
    nice (-(PRIORITY));
#endif
#endif /* HAVE_SETPRIORITY */

    swap_re_uids ();	/* and back to user */

    priority_set = 0;
    return(0);
}

static int swap_re_uids (void)
{
    uid_t hold;
    static int flipflop = 0;

#ifdef HAVE_SETREUID
    setreuid ((int)geteuid(), (int)getuid());
#else
#ifdef HAVE_SETRUID
#ifdef HAVE_SETEUID
    /* should we be turning off interupts here? */

    /* first time thru  Effective will be 0 */
    if (! flipflop)
    {
	/* set Real to 0 */
	hold = getuid ();
	setruid (0);
	seteuid (hold);

	flipflop = 1;
    }
    else
    {
	/* set Effective to 0 */
	hold = geteuid ();
	seteuid (0);
	setruid (hold);
	flipflop = 0;
    }
#endif /* HAVE_SETEUID */
#endif /* HAVE_SETRUID */
#endif /* HAVE_SETREUID */
    return 0;
}

/* returns -1 if cannot create a user other than root */
int set_uid_to_user ()
{
    uid_t user;

    user =  geteuid ();
    if (!user)
	user = getuid ();
    if (!user)
    {
	fprintf (stderr, "Set_uid_to_user () failed!\n");
	return (-1);
    }

    setuid (user);
    return (0);
}


/* leaving this around to point out that the masscomp
**  getuid and geteuid were backwards
*/
/*
#ifdef XMASSCOMP
    user =  getuid ();
    if (!user)
	user = geteuid ();
#else
    user =  geteuid ();
    if (!user)
	user = getuid ();
#endif
*/
