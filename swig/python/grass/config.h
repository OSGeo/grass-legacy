/* include/config.h.  Generated automatically by configure.  */

/*
 * config.h.in
 */

#ifndef _config_h
#define _config_h

#define GDEBUG 1

/* define if standard C headers exist:  headers are
 * stdlib.h, stdarg.h, string.h, and float.h */

/* define if curses.h exists */
#define HAVE_CURSES_H 1

/* define if keypad in lib[n]curses */
#define HAVE_KEYPAD 1

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

/* define if zlib.h exists */
#define HAVE_ZLIB_H 1

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

/* define if g2c.h exists */
#define HAVE_G2C_H 1

/* define if f2c.h exists */
/* #undef HAVE_F2C_H */

/* define gid_t type */
/* #undef gid_t */

/* define off_t type */
/* #undef off_t */

/* define uid_t type */
/* #undef uid_t */

/* Define the return type of signal handlers */
#define RETSIGTYPE void

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

/* define if drand48() exists */
#define HAVE_DRAND48 1

/* define if nanosleep() exists */
#define HAVE_NANOSLEEP 1

/* define if asprintf() exists */
#define HAVE_ASPRINTF 1

/* define if postgres is to be used */
#define HAVE_POSTGRES 1

/* define if SQLite is to be used */
#define HAVE_SQLITE 1

/* define if OGR is to be used */
#define HAVE_OGR 1

/* define if postgres client header exists */
#define HAVE_LIBPQ_FE_H 1

/* define if PQcmdTuples in lpq */
#define HAVE_PQCMDTUPLES 1

/* define if ODBC exists */
/* #undef HAVE_SQL_H */

/* define if PNG support exists */
/* #undef HAVE_PNG */

/* define if jpeglib.h exists */
#define HAVE_JPEGLIB_H 1

/* define if fftw3.h exists */
#define HAVE_FFTW3_H 1

/* define if fftw.h exists */
/* #undef HAVE_FFTW_H */

/* define if dfftw.h exists */
/* #undef HAVE_DFFTW_H */

/* define if BLAS exists */
/* #undef HAVE_LIBBLAS */

/* define if LAPACK exists */
/* #undef HAVE_LIBLAPACK */

/* define if dbm.h exists */
/* #undef HAVE_DBM_H */

/* define if readline exists */
/* #undef HAVE_READLINE_READLINE_H */

/* define if ft2build.h exists */
#define HAVE_FT2BUILD_H 1

/* Whether or not we are using G_socks for display communications */
#define USE_G_SOCKS 1

/* define if X is disabled or unavailable */
/* #undef X_DISPLAY_MISSING */

/* define if libintl.h exists */
#define HAVE_LIBINTL_H 1

/* define if iconv.h exists */
#define HAVE_ICONV_H 1

/* define if NLS requested */
#define USE_NLS 1

/* define if <GL/GLwMDrawA.h> exists */
#define HAVE_GL_GLWMDRAWA_H 1

/* define if <X11/GLw/GLwMDrawA.h> exists */
/* #undef HAVE_X11_GLW_GLWMDRAWA_H */

/* define if TclTk exists */
#define HAVE_TCLTK 1

/* define if putenv() exists */
#define HAVE_PUTENV 1

/* define if setenv() exists */
#define HAVE_SETENV 1

/* define if glXCreatePbuffer exists */
#define HAVE_PBUFFERS 1

/* define if glXCreateGLXPixmap exists */
#define HAVE_PIXMAPS 1

/* define if FFMPEG library exists */
/* #undef HAVE_FFMPEG */

/*
 * configuration information solely dependent on the above
 * nothing below this point should need changing
 */

#if defined(HAVE_VALUES_H) && !defined(HAVE_LIMITS_H)
#define INT_MIN -MAXINT
#endif



/*
 * Defines needed to get large file support - from cdrtools-2.01
 */
#ifdef	USE_LARGEFILES

/* #undef	HAVE_LARGEFILES */

#ifdef	HAVE_LARGEFILES		/* If we have working largefiles at all	   */
				/* This is not defined with glibc-2.1.3	   */

/* #undef _FILE_OFFSET_BITS */	/* # of bits in off_t if settable	   */
/* #undef _LARGEFILE_SOURCE */	/* To make ftello() visible (HP-UX 10.20). */
/* #undef _LARGE_FILES */		/* Large file defined on AIX-style hosts.  */
/* #undef _XOPEN_SOURCE */		/* To make ftello() visible (glibc 2.1.3). */
				/* XXX We don't use this because glibc2.1.3*/
				/* XXX is bad anyway. If we define	   */
				/* XXX _XOPEN_SOURCE we will loose caddr_t */

/* #undef HAVE_FSEEKO */		/* Do we need this? If HAVE_LARGEFILES is  */
				/* defined, we have fseeko()		   */

#endif	/* HAVE_LARGEFILES */
#endif	/* USE_LARGEFILES */

/* define if langinfo.h exists */
#define HAVE_LANGINFO_H 1

#endif /* _config_h */
