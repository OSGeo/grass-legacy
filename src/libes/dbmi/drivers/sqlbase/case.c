#include "globals.h"

void
string_to_lowercase (s)
    char *s;
{
    while (*s)
	char_to_lowercase (s++);
}

void
string_to_uppercase (s)
    char *s;
{
    while (*s)
	char_to_uppercase (s++);
}

void
char_to_lowercase (s)
    char *s;
{
    if (*s >= 'A' && *s <= 'Z')
	*s -= 'A' - 'a' ;
}

void
char_to_uppercase (s)
    char *s;
{
    if (*s >= 'a' && *s <= 'z')
	*s += 'A' - 'a' ;
}
