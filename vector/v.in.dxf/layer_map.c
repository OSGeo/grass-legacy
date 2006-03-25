#include <string.h>
#include "global.h"

/* gotta change this to be malloced */
#define MAX_ALLOC 100

static int add_layer(char *, char *[][2], int *);
static int _add_layer(char *[][2], int *, char *, char *);

static char *line_list[MAX_ALLOC][2];
static int num_lines = 0;
#ifdef LABEL
static char *label_list[MAX_ALLOC][2];
static int num_labels = 0;
#endif

char *remap(char *str, int type)
{
    int i;

    if (!from_table)
	return str;

#ifdef LABEL
    /* do lookups based on label remapping */
    if (type == DXF_LABEL_LINE)
	type = DXF_LABEL;

    if (type == DXF_ASCII) {
	for (i = 0; i < num_lines; i++)
	    if (!strcmp(str, line_list[i][0]))
		return line_list[i][1];
    }
    else {
	for (i = 0; i < num_labels; i++)
	    if (!strcmp(str, label_list[i][0]))
		return label_list[i][1];
    }
#else
    for (i = 0; i < num_lines; i++)
	if (!strcmp(str, line_list[i][0]))
	    return line_list[i][1];
#endif

    return NULL;
}

int add_line_layer(char *str)
{
    return add_layer(str, line_list, &num_lines);
}

#ifdef LABEL
int add_att_layer(char *str)
{
    return add_layer(str, label_list, &num_labels);
}
#endif

static int add_layer(char *str, char *list[][2], int *num)
{
    char buf[200], *buf_p, *p = NULL;

    strcpy(buf, str);
    G_squeeze(buf);
    p = G_index(buf, ':');
    if (*buf == '!') {		/* not to be written out */
	p = NULL;
	buf_p = buf + 1;
    }
    else {
	buf_p = buf;
	if (p != NULL) {	/* have an alias for output */
	    *p = 0;
	    p++;
	}
	else
	    p = buf_p;		/* output is same as original layer */
    }

    return _add_layer(list, num, buf_p, p);
}

static int _add_layer(char *list[][2], int *num, char *from, char *to)
{
    list[*num][0] = G_store(from);
    if (to == NULL)
	list[*num][1] = NULL;
    else
	list[*num][1] = G_store(to);
    (*num)++;

    return *num;
}
