#include <stdio.h>

extern FILE *out;
static int charlist = 0;
static int width = 0;
static char prefix[100];

message(x,y)    char *x,*y;
{
    fprintf(out,"?MESSAGE(%s%s)\n", x,y);
}

assign(x)   char *x;
{
    char dataset[1024];

    sprintf(dataset,"%s%s", prefix, x);
    message(". ",dataset);

    fprintf(out,"%s <- ", dataset);
    width = strlen(dataset) + 4;
}

ask_prefix()
{
    char buf[1024];
    *prefix = 0;
    do
	printf ("Enter a prefix for the S datasets -->");
    while (!G_gets(buf)) ;
    G_strip (buf);
    if (*buf)
	sprintf (prefix, "%s.", buf);
}

begin_char_vector ()
{
    charlist = 1;
    begin_list();
}
begin_numeric_vector ()
{
    charlist = 0;
    begin_list();
}
begin_char_array ()
{
    charlist = 1;
    begin_array();
}
begin_numeric_array ()
{
    charlist = 0;
    begin_array();
}
begin_char_matrix ()
{
    charlist = 1;
    begin_matrix();
}
begin_numeric_matrix ()
{
    charlist = 0;
    begin_matrix();
}
begin_list()
{
    width += 2;
    fprintf(out,"c(");
}
list (x)    char *x;
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
}
end_list ()
{
    fprintf(out,"NULL)");
    width += 5;
}

begin_array ()
{
    fprintf(out, "array(");
    width += 6;
    begin_list();
}

begin_matrix ()
{
    fprintf(out, "matrix(");
    width += 7;
    begin_list();
}

end_array (dim1, dim2, dim3)
{
    end_list();
    fprintf(out,",c(%d,%d,%d))\n", dim1, dim2, dim3);
    width = 0;
}
end_matrix (ncol, nrow, byrow)
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
}
end_vector()
{
    end_list();
    fprintf(out,"\n");
    width = 0;
}
