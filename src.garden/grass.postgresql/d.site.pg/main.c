/*
 *   d.site.inf
 *
 *
 *   Display site(s) locations associated the results of a
 *   database query.
 *
 *   jaf 2/3/92
 */

/*  d.site.pg
      after mods in '94
      modifications 11/98
        to support v6.4 of Postgress
        use libpq instead of psql
        use pghost,pgdbase
        allow cats support through interface if three columns in query
 
   2001-06-26:  Rearrange parser interface so program behaves better,
   		remove some cruft. 
		-- Eric G. Miller <egm2@jps.net>
*/


#include <stdio.h>
#include <limits.h>
#include "gis.h"
#include "dbsite.h"

#ifndef ARG_MAX
#ifdef _POSIX_ARG_MAX
#define ARG_MAX _POSIX_ARG_MAX
#else
#define ARG_MAX 4092
#endif
#endif

#define MAIN

int main(int argc, char **argv)
{
    FILE *fp;
    char   *SQL_stmt = NULL;
    size_t stmt_sz;
    int i;
    struct Option *sql, *map, *plot ;
    struct Option  *tab, *coordx, *coordy, *cats,*where;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
	
    module              =    G_define_module();
    module->description = "Query sites in a PostgreSQL database using an SQL file "\
            "or by specifying conditions directly.  Optionally, plot display or "
            "output to a sites_list \"map\"";

    sql = G_define_option() ;
    sql->key        = "sql" ;
    sql->key_desc   = "file" ;
    sql->type       = TYPE_STRING ;
    sql->required   = NO  ;
    sql->multiple   = NO ;
    sql->description= "SQL statements specifying selection criteria";

    tab = G_define_option() ;
    tab->key        = "table" ;
    tab->type       = TYPE_STRING ;
    tab->required   = NO ;
    tab->multiple   = NO ;
    tab->description= "Table with X,Y coordinates" ;


    coordx = G_define_option() ;
    coordx->key        = "coordx" ;
    coordx->type       = TYPE_STRING ;
    coordx->required   = NO  ;
    coordx->multiple   = NO ;
    coordx->answer     = "X";
    coordx->description= "Column with X-coordinate (required if \"table\" is specified)" ;

    coordy = G_define_option() ;
    coordy->key        = "coordy" ;
    coordy->type       = TYPE_STRING ;
    coordy->required   = NO  ;
    coordy->multiple   = NO ;
    coordy->answer     = "Y";
    coordy->description= "Column with Y-coordinate (required if \"table\" is specified)" ;

    cats = G_define_option() ;
    cats->key        = "cats" ;
    cats->type       = TYPE_STRING ;
    cats->required   = NO  ;
    cats->multiple   = NO ;
    cats->description= "Column with categories" ;

    where = G_define_option() ;
    where->key        = "where" ;
    where->type       = TYPE_STRING ;
    where->required   = NO  ;
    where->multiple   = NO ;
    where->description= "Clause for SQL query (e.g. obj='huts', num > 56, etc.):" ;

    map = G_define_option() ;
    map->key        = "map" ;
    map->type       = TYPE_STRING ;
    map->required   = NO  ;
    map->multiple   = NO ;
    map->description= "New site map name";


    plot = G_define_option() ;
    plot->key         = "plot" ;
    plot->type        = TYPE_STRING ;
    plot->required    = NO  ;
    plot->multiple    = NO ;
    plot->key_desc    = "color,icon,size" ;
    plot->answer      = "gray,x,3" ;
    plot->description = "Colors:red,orange,yellow,green,blue,indigo,violet,magenta,brown,gray,white,black;Icon:diamond, box, plus, x; Size: 1-9. ";

    /* Invoke parser */
    if (G_parser(argc, argv))
        exit(-1);

     /* Check DATABASE env variable */
    if (G__getenv("PG_DBASE") == NULL) {
        fprintf(stderr,
              "Please run g.select.pg to identify a current database.\n");
        exit(-1);
    }

    /* use sql file preferentially */
    if (sql->answer != NULL)
    {
      
        if((fp = fopen(sql->answer,"r")) == NULL) {
            G_fatal_error ("File open error on %s\n",sql->answer);
            exit(-1);
        }
        
        for (stmt_sz = 1024; stmt_sz < ARG_MAX; stmt_sz += 1024)
        {
            SQL_stmt = G_realloc (SQL_stmt, stmt_sz);
            if ((i = fread(SQL_stmt,stmt_sz,1,fp)) < stmt_sz || i < 0)
                break;
        }
        
         /* read all lines of sql stmt into a var  */
        fclose (fp);
        
        i = runInfxFile( SQL_stmt, map->answer, plot->answers );
    }
    else if (tab->answer != NULL && coordx->answer != NULL 
             && coordy->answer != NULL)
    {
        /*************** INFX driver code begins ***************/
        i = buildInfxQry(tab->answer,coordx->answer,coordy->answer,
                cats->answer,where->answer,
                map->answer, NULL, plot->answers);

    }
    else
        G_fatal_error ("Need either an SQL file or all of \"table\", "\
                    "\"coordx\", and \"coordy\"");

    return i;
}


