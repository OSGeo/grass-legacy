#include <stdlib.h>
#include <string.h>
#include "global.h"

static char *dxf_fgets(char *, int, struct dxf_file *);

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

/* returns a zero if header not found, returns a 1 if found */
int dxf_find_header(struct dxf_file *dxf)
{
    while (dxf_get_code(dxf) != -2) {
	/* Some dxf files will not have header information */
	if (strcmp(dxf_buf, "HEADER") == 0 || strcmp(dxf_buf, "ENTITIES") == 0)
	    return strcmp(dxf_buf, "HEADER") == 0;
    }

    G_fatal_error(_("end of file while looking for HEADER"));

    return -1;
}

/* returns a DXF code
 * -1 if non-numeric
 * -2 on EOF
 */
int dxf_read_code(struct dxf_file *dxf, char *buf, int size)
{
    if (!dxf_fgets(buf, size, dxf))
	return -2;

    if (buf[0] >= '0' && buf[0] <= '9') {
	int code = atoi(buf);

	if (!dxf_fgets(buf, size, dxf))
	    return -2;
	return code;
    }

    return -1;	/* not numeric */
}

static char *dxf_fgets(char *buf, int size, struct dxf_file *dxf)
{
    char *p;

    if ((p = fgets(buf, size, dxf->fp))) {
	dxf->pos += strlen(p);
	big_percent(dxf->pos, dxf->size, dxf->percent);
	G_squeeze(buf);
    }

    return p;
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
