/*
bcd4000
**************************************************
*        CONTROL DATA PROPRIETARY PRODUCT        *
*       Copyright Control Data 1990, 1991        *
*                                                *
* Accessed software also protected by copyright. *
**************************************************
ecd4000
 */
/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header$ */

#ifndef	_POSIX_STDLIB_
#define	_POSIX_STDLIB_	1

#ifndef	NULL
#define	NULL	0
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned int	size_t;
#endif

#ifndef _WCHAR_T
#define _WCHAR_T
typedef	long		wchar_t;
#endif

typedef	struct { int quot; int rem; }   div_t;	/* type returned by div() */
typedef	struct { long quot; long rem; } ldiv_t;	/* type returned by ldiv() */

#define	EXIT_SUCCESS	0
#define	EXIT_FAILURE	(-1)

#define	RAND_MAX	0x7fff
#define	MB_CUR_MAX	(sizeof(char))

extern	int		abs();
extern	double		atof(); 
extern	double		strtod(); 
extern	int		atoi(); 
extern	long int	atol();
extern 	int		rand(), srand(); 
extern 	char		*malloc(), *calloc(), *realloc();
extern 	void		free();
extern 	void		abort(), exit();
extern	char		*getenv();
extern	char		*bsearch();
extern	void		qsort();
#endif	_POSIX_STDLIB_
