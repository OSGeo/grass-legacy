#include "gis.h"

/* returns 0, ok
 * -1 error writing
 */

int G_fwrite_key_value (
    FILE *fd,
    struct Key_Value *kv)
{
    int n;
    int err;

    err = 0;
    for (n = 0; n < kv->nitems; n++)
	if (kv->value[n][0])
	{
	    if (EOF == fprintf (fd, "%s: %s\n", kv->key[n], kv->value[n]))
		err = -1;
	}
    return err;
}

struct Key_Value *
G_fread_key_value (fd)
    FILE *fd;
{
    struct Key_Value *kv;
    char *key, *value;
    char buf[1024];

    kv = G_create_key_value();
    if (kv == NULL)
	return NULL;
    while (G_getl(buf, sizeof buf, fd) != 0)
    {
	key = value = buf;
	while (*value && *value != ':')
	    value++;
	if (*value != ':')
	    continue;
	*value++ = 0;
	G_strip(key);
	G_strip(value);
	if(!G_set_key_value (key, value, kv))
	{
	    G_free_key_value(kv);
	    return NULL;
	}
    }
    return kv;
}
