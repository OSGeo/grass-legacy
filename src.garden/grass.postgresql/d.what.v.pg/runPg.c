#include "gis.h"
#include "what.h"
#include <libpq-fe.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "glocale.h"

int runPg(SQL_stmt, print_out)
     char *SQL_stmt;
     char *print_out;

{
    char buf[QRY_LENGTH];

    char sqlcmd[QRY_LENGTH];
    int i, j, nrows, nfields;

    PGconn *pg_conn;
    PGresult *res;
    char *pghost;
    int vert = 0;

    memset(buf, '\0', sizeof(buf));
    memset(sqlcmd, '\0', sizeof(sqlcmd));

    snprintf(sqlcmd, QRY_LENGTH, "%s", SQL_stmt);

    if (!strncmp(print_out, "v", 1))
	vert = 1;

    fprintf(stderr, "\n\nExecuting\n%s\n---------------------\n", sqlcmd);

    pghost = G__getenv("PG_HOST");
    pg_conn = PQsetdb(pghost, NULL, NULL, NULL, G_getenv("PG_DBASE"));

    if (PQstatus(pg_conn) == CONNECTION_BAD) {
	fprintf(stderr, _("Error: connect Postgres:%s\n"),
		PQerrorMessage(pg_conn));
	PQfinish(pg_conn);
	exit(-1);
    }

    res = PQexec(pg_conn, sqlcmd);
    if (!strncmp(sqlcmd, "select", 6) || !strncmp(sqlcmd, "SELECT", 6)) {

	if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	    fprintf(stderr, _("Error: select Postgres:%s\n"),
		    PQerrorMessage(pg_conn));
	    PQclear(res);
	    PQfinish(pg_conn);
	    exit(-1);
	}

	nfields = PQnfields(res);
	nrows = PQntuples(res);

	if (nrows == 1 && vert) {
	    for (j = 0; j < nfields; j++) {
		strncpy(buf, PQgetvalue(res, 0, j), QRY_LENGTH);
		fprintf(stderr,  "%10s I %s\n", PQfname(res, j), buf);
	    }
	}
	else if (nrows) {
	    if (h_num) {
		printf("%s", PQfname(res, 0));
		for (j = 1; j < nfields; j++) {
		    printf( ",%s", PQfname(res, j));
		}
		printf( "\n");
		h_num = 0;
	    }
	    for (i = 0; i < nrows; i++) {
		for (j = 0; j < nfields; j++) {
		    strncpy(buf, PQgetvalue(res, i, j), QRY_LENGTH);
		    printf("%s,", buf);
		}
		printf("\n");
	    }
	}

	fprintf(stderr, _("\n%d rows selected\n\n"), nrows);
    }
    else {

	if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	    fprintf(stderr, _("Error:%s\n"), PQerrorMessage(pg_conn));
	    PQclear(res);
	    PQfinish(pg_conn);
	    exit(-1);
	}
    }

    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */

    PQfinish(pg_conn);
    /* close connection to database */

    return 0;
}
