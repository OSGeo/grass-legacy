#include "gis.h"
#define RULE struct _rule_
RULE
{
    CELL new;
    CELL lo;
    CELL hi;
    RULE *next;
};
