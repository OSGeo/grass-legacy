#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "local_proto.h"

extern FILE *out;
static int charlist = 0;
static int width = 0;
static char prefix[100];

int message (char *x, char *y)
{
    fprintf(out,"?MESSAGE(%s%s)\n", x,y);

    return 0;
}

int assign (char *x)
{
    char dataset[1024];

    sprintf(dataset,"%s%s", prefix, x);
    message(". ",dataset);

    fprintf(out,"%s <- ", dataset);
    width = strlen(dataset) + 4;

    return 0;
}

int ask_prefix (void)
{
    char buf[1024];
    *prefix = 0;
    do
	fprintf (stdout,"Enter a prefix for the S datasets -->");
    while (!G_gets(buf)) ;
    G_strip (buf);
    if (*buf)
	sprintf (prefix, "%s.", buf);

    return 0;
}

int begin_char_vector (void)
{
    charlist = 1;
    begin_list();

    return 0;
}

int begin_numeric_vector (void)
{
    charlist = 0;
    begin_list();

    return 0;
}

int begin_char_array (void)
{
    charlist = 1;
    begin_array();

    return 0;
}

int begin_numeric_array (void)
{
    charlist = 0;
    begin_array();

    return 0;
}

int begin_char_matrix (void)
{
    charlist = 1;
    begin_matrix();

    return 0;
}

int begin_numeric_matrix (void)
{
    charlist = 0;
    begin_matrix();

    return 0;
}

int begin_list (void)
{
    width += 2;
    fprintf(out,"c(");

    return 0;
}

int list (char *x)
{
    int len;

    len = strlen(x) + 1;
    if (charlist)
        len += 2;
    if (width + len > 70)
    {
        fprintf(out,"\n");
        width = 0;
    }
    if (charlist)
        fprintf(out,"'");
    fprintf(out,"%s",x);
    if (charlist)
        fprintf(out,"'");
    fprintf(out,",");
    width += len;

    return 0;
}

int end_list (void)
{
    fprintf(out,"NULL)");
    width += 5;

    return 0;
}

int begin_array (void)
{
    fprintf(out, "array(");
    width += 6;
    begin_list();

    return 0;
}

int 
begin_matrix (void)
{
    fprintf(out, "matrix(");
    width += 7;
    begin_list();

    return 0;
}

int end_array (int dim1, int dim2, int dim3)
{
    end_list();
    fprintf(out,",c(%d,%d,%d))\n", dim1, dim2, dim3);
    width = 0;

    return 0;
}

int 
end_matrix (int ncol, int nrow, int byrow)
{
    end_list();
    if (ncol > 0)
        fprintf(out, ",ncol=%d", ncol);
    if (nrow > 0)
        fprintf(out, ",nrow=%d", nrow);
    if (byrow)
        fprintf(out, ",byrow=T");
    fprintf(out,")\n");
    width = 0;

    return 0;
}

int 
end_vector (void)
{
    end_list();
    fprintf(out,"\n");
    width = 0;

    return 0;
}
