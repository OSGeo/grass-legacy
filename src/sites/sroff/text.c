#include "text.h"

/* text list manipulation routines
 *
 * initialize_text_list:   marks text list as empty
 * free_text_list:         releases all storage allocated for the list
 * add_text:               adds new text to the list
 * rewind_text_list:       moves pointer to first text in list
 * next_text:              get info for next text , advance pointer
 * output_text:		   copy text from list to stdout
 */

add_text (text_list, buf)

	TEXT_LIST *text_list;
	char *buf;
{
	TEXT *new;
	char *xalloc();

	new = (TEXT *) xalloc (sizeof(TEXT));
	new -> buf  = xalloc (strlen(buf)+1);
	strcpy (new->buf, buf);
	new -> next  = 0;

	if (text_list -> last)
		text_list -> last -> next = new;
	text_list -> last = new;
	if ( ! text_list -> first)
		text_list -> first = new;
}

initialize_text_list (text_list)

	TEXT_LIST *text_list;
{
	text_list -> first = 0;
	text_list -> last  = 0;
	text_list -> cur   = 0;
}

rewind_text_list(text_list)

	TEXT_LIST *text_list;
{
	text_list -> cur = text_list -> first;
}

next_text (text_list, buf)

	TEXT_LIST *text_list;
	char **buf;
{
	if ( ! text_list -> cur)
		return 0;

	*buf   = text_list -> cur -> buf;

/* advance the list */
	text_list -> cur = text_list -> cur -> next;

	return 1;
}

free_text_list(text_list)

	TEXT_LIST *text_list;
{
	while (text_list -> first)
	{
		text_list -> cur = text_list -> first -> next;
		free (text_list -> first -> buf);
		free (text_list -> first);
		text_list -> first = text_list -> cur;
	}
	text_list -> last = 0;
}

count_text (text_list)

	TEXT_LIST *text_list;
{
	int n;
	char *buf;

	rewind_text_list (text_list);
	for (n = 0; next_text (text_list, &buf); n++)
		;
	rewind_text_list (text_list);
	return n;
}

output_text (text_list)

	TEXT_LIST *text_list;
{
	char *buf;

	rewind_text_list (text_list);
	while (next_text(text_list, &buf))
		output (buf);
	rewind_text_list (text_list);
}
