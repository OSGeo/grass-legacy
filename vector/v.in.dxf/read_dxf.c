#include "global.h"

/*
 * Reads next line of input file
 * returns atoi of line, or  -1 if NON-numeric  or -2 on EOF
 */

int dxf_readcode(FILE * dxf_file)
{
    char buf[256], *p;
    int ready = 0;

    if (NULL == dxf_fgets(buf, 256, dxf_file))
	return -2;
    for (p = buf; *p; p++) {
	if (*p != ' ' && *p != '\t')
	    ready = 1;
	if (ready) {
	    if ('0' <= *p && *p <= '9')
		return atoi(buf);
	    else
		return -1;	/* NOT NUMERIC */
	}
    }
    return -1;			/* NOT NUMERIC */
}

char *dxf_fgets(char *buf, int size, FILE * fp)
{
    char *p;
    static unsigned long current_size = 0;

    p = fgets(buf, size, fp);
    if (p != NULL) {
	current_size += strlen(p);
	big_percent(current_size, file_size, percent);	/* reporting status of job */
	G_squeeze(buf);
    }
    return p;
}

/* returns a zero if header not found, returns a 1 if found */
int dxf_find_header(FILE * dxf_file)
{
    dxf_fgets(dxf_line, 256, dxf_file);
    /* Some dxf files will not have header information */
    while (strcmp(dxf_line, header) != 0 && strcmp(dxf_line, entitie) != 0) {
	dxf_fgets(dxf_line, 256, dxf_file);
	if (feof(dxf_file)) {
	    fprintf(stderr, "end of file while looking");
	    fprintf(stderr, " for HEADER\n");
	    exit(-1);
	}
    }
    if (strcmp(dxf_line, header) == 0)
	return 1;
    return 0;
}

int dxf_find_entities(FILE * dxf_file)
{
    dxf_fgets(dxf_line, 256, dxf_file);
    while (strcmp(dxf_line, entitie) != 0) {
	dxf_fgets(dxf_line, 256, dxf_file);
	if (feof(dxf_file)) {
	    fprintf(stderr, "end of file while looking");
	    fprintf(stderr, " for ENTITIES\n");
	    return -1;
	}
    }
    return 0;
}

/***************************  big_percent  **********************************/
/* this is a modified version of G_percent created because of the
 * use of unsigned long ints which G_percent does not use
 */
int big_percent(unsigned long n, unsigned long d, int s)
{
    unsigned long x;
    static unsigned long prev = (unsigned long)-1;

    x = n * 100 / d;
    if (x % s)
	return 1;
    if (n <= 0 || n >= d || x != prev) {
	prev = x;
	fprintf(stderr, "%4ld%%\b\b\b\b\b", x);
	fflush(stderr);
    }
    if (x >= 100) {
	fprintf(stderr, "\n");
	prev = (unsigned long)-1;
    }

    return 0;
}
