#include "gis.h"
#include <string.h>

/* -1 can't open file for reading
 * -2 no memory for key,value info, file not modified
 * -3 can't open file for re-write
 * -4 error writing the file (might be damaged)
 */
int G_update_key_value_file (char *file,char *key,char *value)
{
    struct Key_Value *kv;
    int stat;

    kv = G_read_key_value_file (file, &stat);
    if (stat != 0)
	return stat;

    if(!G_set_key_value (key, value, kv))
    {
	G_free_key_value(kv);
	return -2;
    }

    G_write_key_value_file (file, kv, &stat);
    G_free_key_value(kv);

    return stat;
}

/* returns: <0 are file/memory errors
 *           0 not found
 *           1 ok
 */
int G_lookup_key_value_from_file(
    char *file,
    char *key,
    char value[],
    int n)
{
    struct Key_Value *kv;
    int stat;
    char *v;

    *value = 0;
    kv = G_read_key_value_file (file, &stat);
    if (stat != 0)
	return stat;
    
    v = G_find_key_value (key, kv);
    if (v)
    {
	strncpy (value, v, n);
	value[n-1] = 0;
	stat = 1;
    }
    else
	stat = 0;
    G_free_key_value (kv);
    return stat;
}
