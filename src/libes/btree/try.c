#include "btree.h"
main()
{
    char key[100], data[100];
    char *k, *d;
    BTREE B;
    int strcmp();

    btree_create (&B, strcmp, 10);
    while (1)
    {
	printf ("enter key (or RETURN if done): ");
	if (!gets(key)) exit(0);
	if (*key == 0) break;
	printf ("    ");
	if (btree_find (&B,key,&d))
	    printf ("%s = %s\n", key, d);
	else
	    printf ("%s - not found\n", key);
	printf ("    ");
	printf ("enter new value (or RETURN if none): ");
	if (!gets(data)) exit(0);
	if (*data)
	    btree_update (&B, key, strlen(key)+1, data, strlen(data)+1);
    }

    printf ("final tree\n");
    btree_rewind (&B);
    while (btree_next (&B, &k, &d))
	printf ("%s:%s\n", k, d);
}
