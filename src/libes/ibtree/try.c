#include "ibtree.h"
main()
{
    char key[100], data[100];
    char *k, *d;
    IBTREE B;
    int strcmp();

    ibtree_create (&B, strcmp, 10);
    while (1)
    {
	fprintf (stdout,"enter key (or RETURN if done): ");
	if (!gets(key)) exit(0);
	if (*key == 0) break;
	fprintf (stdout,"    ");
	if (ibtree_find (&B,key,&d))
	    fprintf (stdout,"%s = %s\n", key, d);
	else
	    fprintf (stdout,"%s - not found\n", key);
	fprintf (stdout,"    ");
	fprintf (stdout,"enter new value (or RETURN if none): ");
	if (!gets(data)) exit(0);
	if (*data)
	    ibtree_update (&B, key, strlen(key)+1, data, strlen(data)+1);
    }

    fprintf (stdout,"final tree\n");
    ibtree_rewind (&B);
    while (ibtree_next (&B, &k, &d))
	fprintf (stdout,"%s:%s\n", k, d);
}
