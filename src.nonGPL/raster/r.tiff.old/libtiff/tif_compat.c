
/*
 * Copyright (c) 1988, 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

/*
 * TIFF Library Compatibility Routines.
 */
#include "machdep.h"
#include "tiffcompat.h"

#if defined(unix) || defined(MSDOS)
#include <sys/stat.h>

long
TIFFGetFileSize(fd)
	int fd;
{
	struct stat sb;

	return (fstat(fd, &sb) == -1 ? 0 : sb.st_size);
}
#endif

#ifdef THINK_C
#undef	ReadOK
#define	ReadOK(fd, buf, size)	(read(fd, (char *)buf, size) == size)
#define	MAX_CHUNK_SIZE	0x7ffff

int
THINK_C_ReadOK(fd, buf, size)
	int fd;
	char *buf;
	long size;
{
	while (size > MAX_CHUNK_SIZE) {
		if (!ReadOK(fd, buf, MAX_CHUNK_SIZE))
			return (0);
		size -= MAX_CHUNK_SIZE;
		buf += MAX_CHUNK_SIZE;
	}
	return (ReadOK(fd, buf, size));
}
#endif
