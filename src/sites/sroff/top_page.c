#include <stdio.h>
#include "glob.h"
#include "local_proto.h"

int top_of_page (void)
{
	char *buf;

	if (pageno++)
		fprintf (stdout,"\f");

	fprintf (stdout,"\n\n");
	lineno = 3;
	rewind_text_list (&title_text);
	if (!next_text(&title_text, &buf))
		buf = "";
	fprintf (stdout,"%-70s  page %d\n", buf, pageno);

	while (next_text(&title_text, &buf))
		output (buf);
	output_text (&divider_text);
	output_text (&block_title_text);

	return 0;
}
