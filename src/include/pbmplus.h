/* pbmplus.h - header file for PBM, PGM, PPM, and PNM
**
** Copyright (C) 1988, 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#ifndef _PBMPLUS_H_
#define _PBMPLUS_H_

#include <sys/types.h>
#include <ctype.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef VMS
#include <perror.h>
#endif

#if defined(USG) || defined(SVR4) || defined(VMS) || defined(__SVR4)
#define SYSV
#endif
#if ! ( defined(BSD) || defined(SYSV) || defined(MSDOS) || defined(AMIGA) )
/* CONFIGURE: If your system is >= 4.2BSD, set the BSD option; if you're a
** System V site, set the SYSV option; if you're IBM-compatible, set MSDOS;
** and if you run on an Amiga, set AMIGA. If your compiler is ANSI C, you're
** probably better off setting SYSV - all it affects is string handling.
*/
#define BSD
/* #define SYSV */
/* #define MSDOS */
/* #define AMIGA */
#endif

/* CONFIGURE: If you have an X11-style rgb color names file, define its
** path here.  This is used by PPM to parse color names into rgb values.
** If you don't have such a file, comment this out and use the alternative
** hex and decimal forms to specify colors (see ppm/pgmtoppm.1 for details).
*/
/* There was some evidence before Netpbm 9.1 that the rgb database macros
   might be already set right now.  I couldn't figure out how, so I changed
   their meanings and they are now set unconditionally.  -Bryan 00.05.03.
*/
#ifdef VMS
#define RGB_DB1 "PBMplus_Dir:RGB.TXT"
#define RGB_DB2 "PBMplus_Dir:RGB.TXT"
#else
#define RGB_DB1 "/usr/lib/X11/rgb.txt"
#define RGB_DB2 "/usr/openwin/lib/rgb.txt"
#endif

/* CONFIGURE: This is the name of an environment variable that tells
** where the color names database is.  If the environment variable isn't
** set, Netpbm tries the hardcoded defaults set above.
*/
#define RGBENV "RGBDEF"    /* name of env-var */

/* CONFIGURE: Normally, PPM handles a pixel as a struct of three grays.
** If grays are stored in bytes, that's 24 bits per color pixel; if
** grays are stored as shorts, that's 48 bits per color pixel.  PPM
** can also be configured to pack the three grays into a single longword,
** 10 bits each, 30 bits per pixel.
**
** If you don't
** need more than 10 bits for each color component, AND you care more about
** memory use than speed, then this option might be a win.  Under these
** circumstances it will make some of the programs use 1.5 times less space,
** but all of the programs will run about 1.4 times slower.
**
*/
/* #define PPM_PACKCOLORS */

/* CONFIGURE: uncomment this to enable debugging checks. */
/* #define DEBUG */

#if ( defined(SYSV) || defined(AMIGA) )

#include <string.h>
/* Before Netpbm 9.1, rand and srand were macros for random and
   srandom here.  This caused a failure on a SunOS 5.6 system, which
   is SYSV, but has both rand and random declared (with different
   return types).  The macro caused the prototype for random to be a
   second prototype for rand.  Before 9.1, Netpbm programs called
   random() and on a SVID system, that was really a call to rand().
   We assume all modern systems have rand() itself, so now Netpbm
   always calls rand() and if we find a platform that doesn't have
   rand(), we will add something here for that platform.  -Bryan 00.04.26
#define random rand
#define srandom(s) srand(s)
extern void srand();
extern int rand();
*/
#ifndef __SASC
#define index(s,c) strchr(s,c)
#define rindex(s,c) strrchr(s,c)
#ifndef _DCC    /* Amiga DICE Compiler */
#define bzero(dst,len) memset(dst,0,len)
#define bcopy(src,dst,len) memcpy(dst,src,len)
#define bcmp memcmp
#endif /* _DCC */
#endif /* __SASC */

#endif /*SYSV or AMIGA*/

#include <fcntl.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
/* 
   Before Netpbm 9.0, atoi() and exit() were declared for everybody
   except MSDOS and AMIGA, and time() and write() were declared for
   everybody except MSDOS, AMIGA, and __osf__.  fcntl.h, time.h, and
   stlib.h were included for MSDOS and AMIGA, and unistd.h was included
   for everyone except VMS, MSDOS, and AMIGA.  With the netbsd patches,
   atoi(), exit(), time(), and write() were not declared for __NetBSD__.

   We're hoping that all current systems have the standard header
   files, and will reinstate some of these explicit declarations if we
   hear otherwise.  

   If it turns out to be this easy, we should just move these inclusions
   to the source files that actually need them.
   
   -Bryan 2000.04.13

extern int atoi();
extern void exit();
extern long time();
extern int write(); 
*/

/* CONFIGURE: On most BSD systems, malloc() gets declared in stdlib.h, on
** system V, it gets declared in malloc.h. On some systems, malloc.h
** doesn't declare these, so we have to do it here. On other systems,
** for example HP/UX, it declares them incompatibly.  And some systems,
** for example Dynix, don't have a malloc.h at all.  A sad situation.
** If you have compilation problems that point here, feel free to tweak
** or remove these declarations.
*/
#ifdef BSD
#include <stdlib.h>
#endif
#if (defined(SYSV) && !defined(VMS))
#include <malloc.h>
#endif
/* extern char* malloc(); */
/* extern char* realloc(); */
/* extern char* calloc(); */

/* CONFIGURE: Some systems don't have vfprintf(), which we need for the
** error-reporting routines.  If you compile and get a link error about
** this routine, uncomment the first define, which gives you a vfprintf
** that uses the theoretically non-portable but fairly common routine
** _doprnt().  If you then get a link error about _doprnt, or
** message-printing doesn't look like it's working, try the second
** define instead.
*/
/* #define NEED_VFPRINTF1 */
/* #define NEED_VFPRINTF2 */

/* CONFIGURE: Some systems don't have strstr(), which some routines need.
** If you compile and get a link error about this routine, uncomment the
** define, which gives you a strstr.
*/
/* #define NEED_STRSTR */

/* CONFIGURE: Set this option if your compiler uses strerror(errno)
** instead of sys_errlist[errno] for error messages.
*/
#define A_STRERROR

/* CONFIGURE: On small systems without VM it is possible that there is
** enough memory for a large array, but it is fragmented.  So the usual
** malloc( all-in-one-big-chunk ) fails.  With this option, if the first
** method fails, pm_allocarray() tries to allocate the array row by row.
*/
/* #define A_FRAGARRAY */

/* CONFIGURE: If your system has the setmode() function, set HAVE_SETMODE.
** If you do, and also the O_BINARY file mode, pm_init() will set the mode
** of stdin and stdout to binary for all Netpbm programs.
*/
/* #define HAVE_SETMODE */

/*
** Some special things for the Amiga.
*/

/* End of configurable definitions. */

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifdef AMIGA
#include <clib/exec_protos.h>
#define getpid(x)   ((long)FindTask(NULL))
#endif /* AMIGA */

#ifdef DJGPP
#define HAVE_SETMODE
#define lstat stat
#endif /* DJGPP */

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#undef abs
#define abs(a) ((a) >= 0 ? (a) : -(a))
#undef odd
#define odd(n) ((n) & 1)


/* Definitions to make PBMPLUS work with either ANSI C or C Classic. */

#if __STDC__
#define ARGS(alist) alist
#else /*__STDC__*/
#define ARGS(alist) ()
#define const
#endif /*__STDC__*/

/*  CONFIGURE: Netpbm uses __inline__ to declare functions that should
    be compiled as inline code.  GNU C recognizes the __inline__ keyword.
    If your compiler recognizes any other keyword for this, you can set
    it here.
*/
#ifndef __GNUC__
#ifndef __inline__
#define __inline__
#endif
#endif


/* Initialization. */

void pm_init ARGS(( int* argcP, char* argv[] ));

void
pm_nextimage(FILE * file, int * const eofP);

/* Variable-sized arrays definitions. */

char** pm_allocarray ARGS(( int cols, int rows, int size ));
char* pm_allocrow ARGS(( int cols, int size ));
void pm_freearray ARGS(( char** its, int rows ));
void pm_freerow ARGS(( char* itrow ));


/* Case-insensitive keyword matcher. */

int pm_keymatch ARGS(( char* str, char* keyword, int minchars ));


/* Maxval arithmetic */

int pm_maxvaltobits (int maxval);
int pm_bitstomaxval (int bits);
unsigned int pm_lcm (const unsigned int x, 
                     const unsigned int y,
                     const unsigned int z,
                     const unsigned int limit);

/* Error handling definitions. */

void pm_message (const char format[], ...);              /* doesn't return */
void pm_error (const char reason[], ...);              /* doesn't return */
void pm_perror (const char reason[]);              /* doesn't return */
void pm_usage (const char usage[]);              /* doesn't return */

/* File open/close that handles "-" as stdin and checks errors. */

FILE* pm_openr ARGS(( char* name ));
FILE* pm_openw ARGS(( char* name ));
void pm_close ARGS(( FILE* f ));
void pm_closer ARGS(( FILE* f ));
void pm_closew ARGS(( FILE* f ));


/* Endian I/O. */

int pm_readbigshort ARGS(( FILE* in, short* sP ));
int pm_writebigshort ARGS(( FILE* out, short s ));
int pm_readbiglong ARGS(( FILE* in, long* lP ));
int pm_writebiglong ARGS(( FILE* out, long l ));
int pm_readlittleshort ARGS(( FILE* in, short* sP ));
int pm_writelittleshort ARGS(( FILE* out, short s ));
int pm_readlittlelong ARGS(( FILE* in, long* lP ));
int pm_writelittlelong ARGS(( FILE* out, long l ));

enum pm_check_code {
    PM_CHECK_OK,
    PM_CHECK_UNKNOWN_TYPE,
    PM_CHECK_TOO_LONG,
    PM_CHECK_UNCHECKABLE,
    PM_CHECK_TOO_SHORT
};

enum pm_check_type {
    PM_CHECK_BASIC
};

void
pm_check(FILE * file, const enum pm_check_type check_type, 
         const unsigned int need_raster_size,
         enum pm_check_code * const retval_p);


/* By making this <> instead of "", we avoid making shhopt.h a dependency
   of every program in the package when we do make dep.
*/
#include <shhopt.h>

void pm_optParseOptions(int *argc, char *argv[],
                        optStruct opt[], int allowNegNum);
void pm_optParseOptions2(int * const argc, char *argv[],
                        const optStruct2 opt, const unsigned long flags);
/* Use pm_optParseOptions instead of optParseOptions in order to use the
   shared Netpbm libraries
*/

/* You can use OPTENTRY to assign a value to a dynamically or automatically
   allocated optStruct structure with minimal typing and easy readability.

   Here is an example:

       unsigned int option_def_index = 0;
       optStruct *option_def = malloc(100*sizeof(optStruct));
       OPTENTRY('h', "help",     OPT_FLAG, &help_flag, 0);
       OPTENTRY(0,   "alphaout", OPT_STRING, &alpha_filename, 0);
*/       

#define OPTENTRY(shortvalue,longvalue,typevalue,outputvalue,flagvalue) {\
    option_def[option_def_index].shortName = (shortvalue); \
    option_def[option_def_index].longName = (longvalue); \
    option_def[option_def_index].type = (typevalue); \
    option_def[option_def_index].arg = (outputvalue); \
    option_def[option_def_index].flags = (flagvalue); \
    option_def[++option_def_index].type = OPT_END; \
    }

char *
pm_arg0toprogname(const char arg0[]);

/* Compatibility stuff */

#ifdef NEED_STRSTR
char *strstr ARGS((char *s1, char *s2));
#endif

#if defined(NEED_VFPRINTF1) || defined(NEED_VFPRINTF2)
int vfprintf ARGS(( FILE* stream, char* format, va_list args ));
#endif /*NEED_VFPRINTF*/

#endif /*_PBMPLUS_H_*/
