#define SELECT  "SELECT (a=count{pg.%s),b=%ssum{pg.%s}, c=%save{pg.%s},d=%smax{pg.%s}, e=%smin{pg.%s} ) "  
#define CHKTYP  "SELECT (pg_type.typname) where pg_attribute.atttypid = pg_type.oid and pg_attribute.attrelid = pg_class.oid and pg_class.relname = \"%s\" and pg_attribute.attname = \"%s\" \n"  
#define FROM    "from pg in %s\n"
#define WHERE   "where pg.%s NOTNULL\n"
