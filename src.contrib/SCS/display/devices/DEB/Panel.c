/* Functions: Panel_save, Panel_restore, Panel_delete
 *
 * Author: Paul Carlson
 */
#include <stdio.h>

static char *ptr;
static int leng;

Panel_save(name, top, bottom, left, right)
char *name;
int top, bottom, left, right;
{

	leng = strlen(name);
	if (leng > 8)
	{	ptr = name + leng;
		while (*--ptr != '.');
		ptr++;
	}
	else ptr = name;
	if (*ptr == '@') put_chr('u');
	else
	{	put_chr('A');
		put_int(strlen(ptr));
		while (*ptr) put_chr(*ptr++);
	}
	put_int(top);
	put_int(bottom);
	put_int(left);
	put_int(right);
	getchar();
}

Panel_restore(name)
char *name;
{

	leng = strlen(name);
	if (leng > 8)
	{	ptr = name + leng;
		while (*--ptr != '.');
		ptr++;
	}
	else ptr = name;
	if (*ptr == '@') put_chr('v');
	else
	{	put_chr('a');
		put_int(strlen(ptr));
		while (*ptr) put_chr(*ptr++);
	}
	getchar();
}


Panel_delete(name)
char *name;
{

	leng = strlen(name);
	if (leng > 8)
	{	ptr = name + leng;
		while (*--ptr != '.');
		ptr++;
	}
	else ptr = name;
	if (*ptr == '@') put_chr('H');
	else
	{	put_chr('I');
		put_int(strlen(ptr));
		while (*ptr) put_chr(*ptr++);
	}
	getchar();
}
