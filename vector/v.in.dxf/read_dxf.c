#include <stdlib.h>
#include <string.h>
#include "global.h"

/* On error, returns -1, otherwise returns 0 */
struct dxf_file *dxf_open(char *file)
{
    struct dxf_file *dxf;

    dxf = (struct dxf_file *)G_malloc(sizeof(struct dxf_file));

    dxf->name = G_store(file);
    if (!(dxf->fp = fopen(file, "r")))
	return NULL;

    /* get the file size so that big_percent() can be used */
    fseek(dxf->fp, 0L, SEEK_END);
    dxf->size = ftell(dxf->fp);
    rewind(dxf->fp);

    dxf->pos = 0;

    if (dxf->size < 500000)
	dxf->percent = 10;
    else if (dxf->size < 800000)
	dxf->percent = 5;
    else
	dxf->percent = 2;

    /* initialize big_percent() */
    big_percent(0, dxf->size, dxf->percent);

    return dxf;
}

void dxf_close(struct dxf_file *dxf)
{
    fclose(dxf->fp);
    G_free(dxf->name);
    G_free(dxf);

    return;
}

/* Reads next line of input file
 * returns atoi of line, or  -1 if NON-numeric  or -2 on EOF
 */
int dxf_readcode(struct dxf_file *dxf)
{
    char buf[256], *p;
    int ready = 0;

    if (!dxf_fgets(buf, 256, dxf))
	return -2;
    for (p = buf; *p; p++) {
	if (*p != ' ' && *p != '\t')
	    ready = 1;
	if (ready) {
	    if ('0' <= *p && *p <= '9')
		return atoi(buf);
	    else
		break;
	}
    }

    return -1;			/* NOT NUMERIC */
}

char *dxf_fgets(char *buf, int size, struct dxf_file *dxf)
{
    char *p;

    if ((p = fgets(buf, size, dxf->fp))) {
	dxf->pos += strlen(p);
	big_percent(dxf->pos, dxf->size, dxf->percent);
	G_squeeze(buf);
    }

    return p;
}

/* returns a zero if header not found, returns a 1 if found */
int dxf_find_header(struct dxf_file *dxf)
{
    dxf_fgets(dxf_buf, 256, dxf);
    /* Some dxf files will not have header information */
    while (strcmp(dxf_buf, "HEADER") != 0 && strcmp(dxf_buf, "ENTITIES") != 0) {
	if (!dxf_fgets(dxf_buf, 256, dxf))
	    G_fatal_error(_("end of file while looking for HEADER"));
    }

    return strcmp(dxf_buf, "HEADER") == 0;
}

/* this is a modified version of G_percent created because of the use of
 * unsigned long ints which G_percent does not use
 */
int big_percent(unsigned long n, unsigned long d, int s)
{
    int x;
    static int prev = -1;

    x = 100 * (double)n / (double)d;
    if (x % s)
	return 1;
    if (n <= 0 || n >= d || x != prev) {
	prev = x;
	fprintf(stderr, "%4d%%\b\b\b\b\b", x);
	fflush(stderr);
    }
    if (x >= 100) {
	fprintf(stderr, "\n");
	prev = -1;
    }

    return 0;
}
