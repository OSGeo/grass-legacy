#include "dbmi.h"


void
db_char_to_lowercase (s)
    char *s;
{
    if (*s >= 'A' && *s <= 'Z')
	*s -= 'A' - 'a' ;
}

void
db_char_to_uppercase (s)
    char *s;
{
    if (*s >= 'a' && *s <= 'z')
	*s += 'A' - 'a' ;
}

void
db_Cstring_to_lowercase (s)
    char *s;
{
    while (*s)
	db_char_to_lowercase (s++);
}

void
db_Cstring_to_uppercase (s)
    char *s;
{
    while (*s)
	db_char_to_uppercase (s++);
}

int
db_nocase_compare (a, b)
    char *a, *b;
{
    char s, t;

    while (*a && *b)
    {
	s = *a++;
	t = *b++;
	db_char_to_uppercase (&s);
	db_char_to_uppercase (&t);
	if (s != t)
	    return 0;
    }
    return (*a == 0 && *b == 0);
}
