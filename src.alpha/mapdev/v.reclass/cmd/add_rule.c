#include "rule.h"

add_rule (tail, lo, hi, new)
    RULE **tail;
    CELL lo, hi, new;
{
    RULE *r;
    r = (RULE *) G_malloc (sizeof(RULE));

    r->lo = lo;
    r->hi = hi;
    r->new = new;
    r->next = NULL;
    if (*tail)
	(*tail)->next = r;
    *tail = r;
}
