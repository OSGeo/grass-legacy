/* src/include/config.h.  Generated automatically by configure.  */

/*
 * config.h.in
 */

#ifndef _config_h
#define _config_h

/* define if standard C headers exist:  headers are
 * stdlib.h, stdarg.h, string.h, and float.h */

/* define if curses.h exists */
#define HAVE_CURSES_H 1

/* define if limits.h exists */
#define HAVE_LIMITS_H 1

/* define if termio.h exists */
#define HAVE_TERMIO_H 1

/* define if termios.h exists */
#define HAVE_TERMIOS_H 1

/* define if unistd.h exists */
#define HAVE_UNISTD_H 1

/* define if values.h exists */
#define HAVE_VALUES_H 1

/* define if sys/ioctl.h exists */
#define HAVE_SYS_IOCTL_H 1

/* define if sys/mtio.h exists */
#define HAVE_SYS_MTIO_H 1

/* define if sys/resource.h exists */
#define HAVE_SYS_RESOURCE_H 1

/* define if sys/time.h exists */
#define HAVE_SYS_TIME_H 1

/* define if time.h and sys/time.h can be included together */
#define TIME_WITH_SYS_TIME 1

/* define if sys/timeb.h exists */
#define HAVE_SYS_TIMEB_H 1

/* define if sys/types.h exists */
#define HAVE_SYS_TYPES_H 1

/* define if sys/utsname.h exists */
#define HAVE_SYS_UTSNAME_H 1

/* define gid_t type */
/* #undef gid_t */

/* define off_t type */
/* #undef off_t */

/* define uid_t type */
/* #undef uid_t */

/* define curses.h WINDOW structure component */
#define CURSES_MAXY _maxy

/* define if ftime() exists */
#define HAVE_FTIME 1

/* define if gethostname() exists */
#define HAVE_GETHOSTNAME 1

/* define if gettimeofday() exists */
#define HAVE_GETTIMEOFDAY 1

/* define if lseek() exists */
#define HAVE_LSEEK 1

/* define if nice() exists */
#define HAVE_NICE 1

/* define if time() exists */
#define HAVE_TIME 1

/* define if uname() exists */
#define HAVE_UNAME 1

/* define if seteuid() exists */
#define HAVE_SETEUID 1

/* define if setpriority() exists */
#define HAVE_SETPRIORITY 1

/* define if setreuid() exists */
#define HAVE_SETREUID 1

/* define if setruid() exists */
/* #undef HAVE_SETRUID */

/* define if setpgrp() takes no argument */
#define SETPGRP_VOID 1

/*
 * configuration information solely dependent on the above
 * nothing below this point should need changing
 */

#if defined(HAVE_VALUES_H) && !defined(HAVE_LIMITS_H)
#define INT_MIN -MAXINT
#endif

#endif /* _config_h */
