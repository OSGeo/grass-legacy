#include <stdlib.h>
#include <gis.h>
#include <dbmi.h>

/* init reclass rule */
dbRclsRule * db_rcls_rule_init( int n )
{
    dbRclsRule *rule;
    rule = (dbRclsRule *) G_malloc ( sizeof(dbRclsRule));
    rule->count = 0;

    rule->alloc = n;
    rule->cat = (int *) G_calloc ( n, sizeof( int ));    
    rule->label = (char **) G_calloc ( n, sizeof(char *));
    rule->where = (char **) G_calloc ( n, sizeof(char *));

    return rule;
}

/* extend reclass rule */
void db_rcls_rule_extend( dbRclsRule *rule, int n )
{
    int i;
     
    if ( n <= rule->alloc )
        return;

    rule->cat = (int *) G_realloc ( rule->cat, n * sizeof( int ));    
    rule->label = (char **) G_realloc ( rule->label, n * sizeof(char *));
    rule->where = (char **) G_realloc ( rule->where, n * sizeof(char *));

    for ( i = rule->alloc ; i < n; i++ )
    {
        rule->cat[i] = 0;
	rule->label[i] = NULL;
	rule->where[i] = NULL;
    }
    rule->alloc = n;

    return;   
}
