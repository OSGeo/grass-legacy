#include <string.h>
#include <stdlib.h>
#include "gis.h"

struct Key_Value *
G_create_key_value()
{
    struct Key_Value *kv;

    kv = (struct Key_Value *) malloc (sizeof(struct Key_Value));
    if (kv == NULL)
	return NULL;

    kv->nitems = 0;
    kv->nalloc = 0;
    kv->key = (char **) NULL;
    kv->value = (char **) NULL;

    return kv;
}

/* if key has spaces in it, this will break the logic 
 * so rule is: NO SPACES IN key
 * returns 0 - no memory
 *         1 - ok, but key was NULL or "" so ignored
 *         2 - ok
 */
int G_set_key_value (
    char *key,char *value,
    struct Key_Value *kv)
{
    int n;
    int size;

    if (key == NULL || key == 0)
	return 1;

    for (n = 0; n < kv->nitems; n++)
	if (strcmp(key, kv->key[n]) == 0)
	    break;
    
    if (n == kv->nitems)
    {
	if (n >= kv->nalloc)
	{
	    if (kv->nalloc <= 0)
	    {
		kv->nalloc = 8;
		size = kv->nalloc * sizeof (char *);
		kv->key = (char **) malloc (size);
		kv->value = (char **) malloc (size);
	    }
	    else
	    {
		kv->nalloc *= 2;
		size = kv->nalloc * sizeof (char *);
		kv->key = (char **) realloc (kv->key, size);
		kv->value = (char **) realloc (kv->value, size);
	    }

	    if (kv->key == NULL || kv->value == NULL)
	    {
		if (kv->key)
		{
		    free (kv->key);
		    kv->key = NULL;
		}
		if (kv->value)
		{
		    free (kv->value);
		    kv->value = NULL;
		}
		kv->nitems = kv->nalloc = 0;
		return 0;
	    }
	}
	kv->value[n] = NULL;
	kv->key[n] = malloc(strlen(key)+1);
	if (kv->key[n] == NULL)
	    return 0;
	strcpy (kv->key[n], key);
	kv->nitems++;
    }
    if (value == NULL)
	size = 0;
    else
	size = strlen(value);
    if (kv->value[n] != NULL)
	free(kv->value[n]);
    if (size > 0)
    {
	kv->value[n] = malloc (size+1);
	if (kv->value[n] == NULL)
	    return 0;
	strcpy (kv->value[n], value);
    }
    else
	kv->value[n] = NULL;
    return 2;
}

char *G_find_key_value (char *key, struct Key_Value *kv)
{
    int n;

    for (n = 0; n < kv->nitems; n++)
	if (strcmp (key, kv->key[n]) == 0)
	    return kv->value[n][0] ? kv->value[n] : NULL;
    return NULL;
}

int G_free_key_value(struct Key_Value *kv)
{
    int n;

    for (n = 0; n < kv->nitems; n++)
    {
	free (kv->key[n]);
	free (kv->value[n]);
    }
    free (kv->key);
    free (kv->value);
    kv->nitems = 0; /* just for safe measure */
    kv->nalloc = 0;
    free (kv);

    return 0;
}
