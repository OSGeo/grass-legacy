/*
 *******************************************************************
 * G_store (string)
 *
 * allocates permanent memory to hold 'string'
 * copies 'string' to this memory, and returns the address
 * of the allocated memory
 *******************************************************************/

char *
G_store (s) char *s;
{
    char *buf;
    char *G_malloc();

    buf = G_malloc (strlen(s) + 1);
    strcpy (buf, s);
    return buf;
}
