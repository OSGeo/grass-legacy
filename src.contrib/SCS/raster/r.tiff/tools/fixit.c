
/*
 * Clobber TIFF directory tags that look like
 * they were written incorrectly by previous
 * versions of the library.
 */
#include <stdio.h>
#include "tiff.h"

char	*curfile;
int	swab;
int	bigendian;
int	typeshift[6];	/* data type shift counts */
long	typemask[6];	/* data type masks */

main(argc, argv)
	char *argv[];
{
	int one = 1, fd;

	bigendian = (*(char *)&one == 0);
	for (argc--, argv++; argc > 0; argc--, argv++) {
		fd = open(argv[0], 2);
		if (fd < 0) {
			perror(argv[0]);
			exit(-1);
		}
		curfile = *argv;
		swab = 0;
		fixit(fd);
		close(fd);
	}
	exit(0);
}

TIFFHeader h;

fixit(fd)
	int fd;
{

	lseek(fd, 0, 0);
	if (read(fd, &h, sizeof (h)) != sizeof (h))
		ReadError("TIFF header");
	/*
	 * Setup the byte order handling.
	 */
	if (h.tiff_magic != TIFF_BIGENDIAN && h.tiff_magic != TIFF_LITTLEENDIAN)
		Fatal("Not a TIFF file, bad magic number %d (0x%x)",
		    h.tiff_magic, h.tiff_magic);
	InitByteOrder(h.tiff_magic);
	/*
	 * Swap header if required.
	 */
	if (swab) {
		TIFFSwabShort(&h.tiff_version);
		TIFFSwabLong(&h.tiff_diroff);
	}
	/*
	 * Now check version (if needed, it's been byte-swapped).
	 * Note that this isn't actually a version number, it's a
	 * magic number that doesn't change (stupid).
	 */
	if (h.tiff_version != TIFF_VERSION)
		Fatal("Not a TIFF file, bad version number %d (0x%x)",
		    h.tiff_version, h.tiff_version); 
	FixDirectory(fd, h.tiff_diroff);
}

#define	ord(e)	((int)e)

/*
 * Initialize shift & mask tables and byte
 * swapping state according to the file
 * byte order.
 */
static
InitByteOrder(magic)
	int magic;
{

	typemask[0] = 0;
	typemask[ord(TIFF_BYTE)] = 0xff;
	typemask[ord(TIFF_SHORT)] = 0xffff;
	typemask[ord(TIFF_LONG)] = 0xffffffff;
	typemask[ord(TIFF_RATIONAL)] = 0xffffffff;
	typeshift[0] = 0;
	typeshift[ord(TIFF_LONG)] = 0;
	typeshift[ord(TIFF_RATIONAL)] = 0;
	if (magic == TIFF_BIGENDIAN) {
		typeshift[ord(TIFF_BYTE)] = 24;
		typeshift[ord(TIFF_SHORT)] = 16;
		swab = !bigendian;
	} else {
		typeshift[ord(TIFF_BYTE)] = 0;
		typeshift[ord(TIFF_SHORT)] = 0;
		swab = bigendian;
	}
}

static int datawidth[] = {
    1,	/* nothing */
    1,	/* TIFF_BYTE */
    1,	/* TIFF_ASCII */
    2,	/* TIFF_SHORT */
    4,	/* TIFF_LONG */
    8,	/* TIFF_RATIONAL */
};

/*
 * Read the next TIFF directory from a file
 * and convert it to the internal format.
 * Fix any tags that might have been written
 * incorrectly by old versions of the library.
 */
FixDirectory(fd, off)
	int fd;
	long off;
{
	register TIFFDirEntry *dp;
	register int n, i;
	TIFFDirEntry *dir;
	short dircount;
	char *cp;
	long v;

	if (off == 0)			/* no more directories */
		return;
	if (lseek(fd, off, 0) != off) {
		Fatal("Seek error accessing TIFF directory");
		return;
	}
	if (read(fd, &dircount, sizeof (short)) != sizeof (short)) {
		ReadError("directory count");
		return;
	}
	if (swab)
		TIFFSwabShort(&dircount);
	dir = (TIFFDirEntry *)malloc(dircount * sizeof (TIFFDirEntry));
	if (dir == NULL) {
		Fatal("No space for TIFF directory");
		return;
	}
	off = lseek(fd, 0, 1);
	if (read(fd, dir, dircount*sizeof (*dp)) != dircount*sizeof (*dp)) {
		ReadError("directory contents");
		goto done;
	}
	for (dp = dir, n = dircount; n > 0; n--, dp++) {
		if (swab) {
			TIFFSwabArrayOfShort(&dp->tdir_tag, 2);
			TIFFSwabArrayOfLong(&dp->tdir_count, 2);
		}
		switch (dp->tdir_tag) {
		case TIFFTAG_ROWSPERSTRIP:
			if (dp->tdir_count == -1)
				dp->tdir_count = 1;
			break;
		case 32768:
			dp->tdir_tag = TIFFTAG_MATTEING;
			break;
		case TIFFTAG_STRIPOFFSETS:
		case TIFFTAG_STRIPBYTECOUNTS:
			if (dp->tdir_count != 1 || dp->tdir_type != TIFF_LONG)
				break;
			/* yech ... patch old bug where value was indirect */
			TIFFFetchData(fd, dp, &v);
			dp->tdir_offset = v;
			break;
		}
	}
	if (lseek(fd, off, 0) != off ||
	    write(fd, dir, dircount*sizeof (*dp)) != dircount*sizeof (*dp))
		Fatal("Error patching TIFF directory");
done:
	if (dir)
		free((char *)dir);
}

/*
 * Fetch a contiguous directory item.
 */
static
TIFFFetchData(fd, dir, cp)
	int fd;
	TIFFDirEntry *dir;
	char *cp;
{
	int cc, w;

	w = datawidth[dir->tdir_type];
	cc = dir->tdir_count * w;
	if (lseek(fd, dir->tdir_offset, 0) == dir->tdir_offset &&
	    read(fd, cp, cc) == cc) {
		if (swab) {
			switch (dir->tdir_type) {
			case TIFF_SHORT:
				TIFFSwabArrayOfShort(cp, dir->tdir_count);
				break;
			case TIFF_LONG:
				TIFFSwabArrayOfLong(cp, dir->tdir_count);
				break;
			case TIFF_RATIONAL:
				TIFFSwabArrayOfLong(cp, 2*dir->tdir_count);
				break;
			}
		}
		return (cc);
	}
	ReadError("data for tag %d", dir->tdir_tag);
	/*NOTREACHED*/
}

TIFFSwabShort(wp)
	unsigned short *wp;
{
	register unsigned char *cp = (unsigned char *)wp;
	int t;

	t = cp[1]; cp[1] = cp[0]; cp[0] = t;
}

TIFFSwabLong(lp)
	unsigned long *lp;
{
	register unsigned char *cp = (unsigned char *)lp;
	int t;

	t = cp[3]; cp[3] = cp[0]; cp[0] = t;
	t = cp[2]; cp[2] = cp[1]; cp[1] = t;
}

TIFFSwabArrayOfShort(wp, n)
	unsigned short *wp;
	register int n;
{
	register unsigned char *cp;
	register int t;

	/* XXX unroll loop some */
	while (n-- > 0) {
		cp = (unsigned char *)wp;
		t = cp[1]; cp[1] = cp[0]; cp[0] = t;
		wp++;
	}
}

TIFFSwabArrayOfLong(lp, n)
	register unsigned long *lp;
	register int n;
{
	register unsigned char *cp;
	register int t;

	/* XXX unroll loop some */
	while (n-- > 0) {
		cp = (unsigned char *)lp;
		t = cp[3]; cp[3] = cp[0]; cp[0] = t;
		t = cp[2]; cp[2] = cp[1]; cp[1] = t;
		lp++;
	}
}

ReadError(what)
	char *what;
{

	Fatal("Error while reading %s", what);
}

Error(fmt, a1, a2, a3, a4, a5)
{

	fprintf(stderr, "%s: ", curfile);
	fprintf(stderr, fmt, a1, a2, a3, a4, a5);
	fprintf(stderr, ".\n");
}

Fatal(fmt, a1, a2, a3, a4, a5)
	char *fmt;
{

	Error(fmt, a1, a2, a3, a4, a5);
	exit(-1);
}
