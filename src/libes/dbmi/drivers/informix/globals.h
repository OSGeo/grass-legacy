#define MAX_DATABASE_NAME 512
#define MAX_STRING_SIZE   256

/* cursors */
typedef struct _cursor {
  char cursor_name[10];
  char statement_name[10];
  char descriptor_name[10];
  char update_name[10];
  char update_descriptor_name[10];
  char update_table_name[19];
  char delete_name[10];
  dbToken token;
  int type;              /* type of cursor: SELECT, UPDATE, INSERT */
} cursor;

cursor * make_cursor();
void free_cursor();

char * get_dbpath();
char * build_dbpath();

void decompose_tablename();
void compose_tablename();

char * informix_engine();
#define STANDARD_ENGINE "se"
#define ONLINE_ENGINE   "online"

/* The max size of the string representation of an Informix DECIMAL type */
/* # digits precision  32 
 * decimal point        1
 * leading +,-          1
 * trailing 'E'         1
 * digits of exponent   3
 * exponent sign        1
 * null terminator      1
 *                    40
 */
#define DECIMAL_STRING_SIZE 100

