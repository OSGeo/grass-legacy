#include "P.h"
Ptext (s) unsigned char *s;
{
    Pouts("SP216;LB"); 
    while (*s >= ' ' && *s < 0177) {
	Poutc (*s++);
    }
    Pouts("\003;CP;");
}
