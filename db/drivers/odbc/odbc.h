#ifndef _ODBC_H_
#define	_ODBC_H_

#ifndef __FreeBSD__

#include <odbc/sql.h>
#include <odbc/sqlext.h>
#include <odbc/sqltypes.h>

#else

/* FreeBSD unixODBC port installs these header files in /usr/local/include */
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#endif

#endif
