#include "dbmi.h"

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_zero(s, n)
    void *s;
{
    char *c = (char *) s;
    while (n-- > 0)
	*c++ = 0;
}
