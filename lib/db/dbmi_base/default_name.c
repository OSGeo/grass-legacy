#include <stdlib.h>
#include "gis.h"
#include "dbmi.h"

/*!
 \fn char * db_get_default_driver ( void )
 \brief returns pointer to default driver name
 \return returns pointer to default driver name or NULL if not set
*/
char *
db_get_default_driver_name ( void )
{
    char *drv;
    char *fakestart;
    
    /* fake session for HTML generation with parser */
    fakestart = getenv( "GRASS_FAKE_START" );
    if ( fakestart == NULL ) {
       if ( (drv = G__getenv2("DB_DRIVER", G_VAR_MAPSET))  )
	  return G_store(drv);
    }

    return NULL;
}

/*!
 \fn char * db_get_default_database ( void )
 \brief returns pointer to default database name
 \return returns pointer to default database name or NULL if not set
*/
char *
db_get_default_database_name ( void )
{
    char *drv;
    char *fakestart;
    
    /* fake session for HTML generation with parser */
    fakestart = getenv( "GRASS_FAKE_START" );
    if ( fakestart == NULL ) {
       if ( (drv = G__getenv2("DB_DATABASE", G_VAR_MAPSET))  )
	  return G_store(drv);
    }

    return NULL;
}

/*!
 \fn char * db_get_default_schema_name ( void )
 \brief returns pointer to default schema name
 \return returns pointer to default schema name or NULL if not set
*/
char *
db_get_default_schema_name ( void )
{
    char *sch;
    
    if ( (  sch = G__getenv2("DB_SCHEMA", G_VAR_MAPSET) )  )
	  return G_store(sch);

    return NULL;
}
