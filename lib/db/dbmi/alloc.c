#include "dbmi.h"

extern char *malloc();
extern char *calloc();
extern char *realloc();

char *
db_store(s)
    char *s;
{
    char *a;

    a = db_malloc (strlen(s) + 1);
    if (a)
	strcpy (a, s);
    return a;
}

void *
db_malloc(n)
    int n;
{
    void *s;

    if (n <= 0)
	n = 1;
    s = malloc((unsigned int) n);
    if (s == NULL)
	db_memory_error ();
    return s;
}

void *
db_calloc(n,m)
    int n;
    int m;
{
    void *s;

    if (n <= 0)
	n = 1;
    if (m <= 0)
	m = 1;
    s = calloc((unsigned int) n, (unsigned int) m);
    if (s == NULL)
	db_memory_error ();
    return s;
}

void *
db_realloc(s,n)
    void *s;
    int n;
{
    if (n <= 0)
	n = 1;
    if(s == NULL)
	s = malloc((unsigned int) n);
    else
	s = realloc (s, (unsigned int) n);
    if (s == NULL)
	db_memory_error ();
    return s;
}
