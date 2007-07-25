/* d.what.s.pg
   runPg.c
   run query in loop or run query once
   major modifications cfa 11/98    */
/* Added visual marks allowing to see better
	what was selected. --alex, nov/02
*/

#include "what.h"
#include "display.h"
#include "raster.h"
#include <libpq-fe.h>
#include <string.h>
#include <stdlib.h>
#include "glocale.h"

int runqry(SQL_stmt, pts, print_out)
     char *SQL_stmt;
     struct Sql *pts;
     char *print_out;
{
    char buf[QRY_LENGTH];
    char sqlcmd[QRY_LENGTH];
    int i, j, nrows, nfields;
    PGconn *pg_conn;
    PGresult *res;
    char *pghost;

    i = 1;

    memset(buf, '\0', sizeof(buf));
    memset(sqlcmd, '\0', sizeof(sqlcmd));

    sprintf(sqlcmd,
	    "%s @ '(%f,%f,%f,%f)'::box", SQL_stmt,
	    pts->minX, pts->minY, pts->maxX, pts->maxY);
    /* use Postgres graphic operators 
       @ operator test point in box 
       cfa 11/98   */

    fprintf(stderr,
	    "\n\nExecuting\n%s;\n clause  @ '( )'::box addded automatically.\n\n",
	    sqlcmd);
    pghost = G__getenv("PG_HOST");

    pg_conn = PQsetdb(pghost, NULL, NULL, NULL, G_getenv("PG_DBASE"));
    if (PQstatus(pg_conn) == CONNECTION_BAD) {
	fprintf(stderr, _("Error: connect Postgres:%s\n"),
		PQerrorMessage(pg_conn));
	PQfinish(pg_conn);
	exit(-1);
    }

    res = PQexec(pg_conn, sqlcmd);
    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	fprintf(stderr, _("Error: select Postgres:%s\n"),
		PQerrorMessage(pg_conn));
	PQclear(res);
	PQfinish(pg_conn);
	exit(-1);
    }

    nfields = PQnfields(res);
    nrows = PQntuples(res);
    if (nrows == 1 && !strncmp(print_out, "v", 1)) {
	for (j = 0; j < nfields; j++) {
	    strcpy(buf, PQgetvalue(res, 0, j));
	    fprintf(stderr, "%10s I %s\n", PQfname(res, j), buf);
	}
    }
    else if (nrows) {
	printf("%s", PQfname(res, 0));
	for (j = 1; j < nfields; j++) {
	    printf(",%s", PQfname(res, j));
	}
	printf("\n");
	for (i = 0; i < nrows; i++) {
	    for (j = 0; j < nfields; j++) {
		strcpy(buf, PQgetvalue(res, i, j));
		printf("%s,", buf);
	    }
	    printf("\n");
	}
    }

    fprintf(stderr, _("\n%d rows selected\n"), nrows);

    PQclear(res);
    /* explicitly close select result to avoid memory leaks  */

    PQfinish(pg_conn);
    /* close connection to database */

    return 0;
}

int runPg(SQL_stmt, str_dist, print_out)
     char *SQL_stmt;
     char *str_dist;
     char *print_out;
{
    int stat = 0, button;
    double searchdist = 0.0;
    struct Sql *pts;
    double atof();
    char *paneli;

    paneli = G_tempfile();

    /* Initialze SQL query structure        */
    pts = (struct Sql *) G_malloc(sizeof(struct Sql));
    G_zero(pts, sizeof(struct Sql));

    searchdist = atof(str_dist);
    if (!searchdist) {
	fprintf(stderr, _("Error converting %s, have %f \n")
		, str_dist, searchdist);
	exit(-1);
    }
/* help I always screw up atof functions ---  look closely and fix it */

    pts->distance = (double) searchdist;

    R_open_driver();
    D_setup(0);
    do {
	R_panel_save(paneli, R_screen_top(), R_screen_bot(),
		     R_screen_left(), R_screen_rite());

	button = getArea(pts);
	if (button != 3)
	    stat = runqry(SQL_stmt, pts, print_out);
	R_panel_restore(paneli);
	R_panel_delete(paneli);

    } while (button != 3);

    R_close_driver();

    return (stat);

}
