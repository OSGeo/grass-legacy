#include <sqlca.h>

sql_eof()
{
    return (sqlca.sqlcode == 100);
}
