#include "glob.h"

top_of_page()
{
	char *buf;

	if (pageno++)
		printf("\f");

	printf("\n\n");
	lineno = 3;
	rewind_text_list (&title_text);
	if (!next_text(&title_text, &buf))
		buf = "";
	printf("%-70s  page %d\n", buf, pageno);

	while (next_text(&title_text, &buf))
		output (buf);
	output_text (&divider_text);
	output_text (&block_title_text);
}
