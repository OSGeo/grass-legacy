#include "sql.h"
is_null_value(fc)
    SQLTFSC fc;
{
    return fc == FETRNUL;
}
