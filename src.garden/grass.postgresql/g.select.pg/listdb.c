#include <stdio.h>
#include <stdlib.h>
#include <libpq-fe.h>
#include "glocale.h"

void listdb(pghost)
     char *pghost;
{

    PGconn *pg_conn;
    PGresult *res;
    int num, i;
/*     char *ok;
 */
    pg_conn = PQsetdb(pghost, NULL, NULL, NULL, "template1");

    if (PQstatus(pg_conn) == CONNECTION_BAD) {
	fprintf(stderr, _("Error: connect Postgres:%s\n"), PQerrorMessage(pg_conn));
	PQfinish(pg_conn);
	exit(-1);
    }

    res = PQexec(pg_conn, "select datname from pg_database");
    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	fprintf(stderr, _("Error: select Postgres:%s\n"), PQerrorMessage(pg_conn));
	PQclear(res);
	PQfinish(pg_conn);
	exit(-1);
    }


/*     ok = "Olga K. is my good friend";
    printf ("%s\n", ok); don't delete this
 */

    num = PQntuples(res);
    for (i = 0; i < num; i++)
	fprintf(stderr, "%s\n", PQgetvalue(res, i, 0));

    PQclear(res);
    PQfinish(pg_conn);

}
