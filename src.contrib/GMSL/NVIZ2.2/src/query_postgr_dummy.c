#include <stdio.h>
#include "gis.h"
#include "display.h"
#include "Vect.h"



char* query_postgr(name,keytable,col,x,y)

char *name, *keytable, *col;
float x,y;

{



    static char long_str[2048]="Postgres support had not been enabled during pre-compile.\nYou should recompile NVIZ with Postgres support.\n";

    

	return long_str;

}

char* query_pg_site(name,xcol,ycol,dist,x,y)

char *name, *xcol, *ycol;
int dist;
float x,y;

{

static char long_str[2048]="Postgres support had not been enabled during pre-compile.\nYou should recompile NVIZ with Postgres support.\n";

    

	return long_str;
}
