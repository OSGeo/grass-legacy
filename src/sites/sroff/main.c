#define MAIN
#include "glob.h"

#define TITLE 1
#define BLOCK 2
#define BLOCK_TITLE 3
#define DIVIDER 4

main()
{
	int state;
	int width;
	int centered;
	char buf[1024];

	initialize_text_list (&block_text);
	initialize_text_list (&block_title_text);
	initialize_text_list (&title_text);
	initialize_text_list (&divider_text);

	pageno = 0;
	pagelen = 63;
	width = 80;
	state = 0;
	centered = 0;

	eject();

	while (gets(buf))
	{
		if (centered)
		{
			center (buf, width);
			centered = 0;
		}
		if (strcmp(buf,".setcol") == 0)
		{
			if (!gets(buf)) break;
			setcols (buf);
			continue;
		}
		if (strcmp(buf,".setcolsep") == 0)
		{
			if (!gets(buf)) break;
			setcolsep (buf);
			continue;
		}
		if (strcmp(buf,".col") == 0)
		{
			columns (buf);
		}
		if (strcmp(buf,".center") == 0)
		{
			centered = 1;
		}
		else if (strcmp(buf,".title") == 0)
		{
			state = TITLE;
			begin_title();
		}
		else if (strcmp(buf,".divider") == 0)
		{
			state = DIVIDER;
			begin_divider();
		}
		else if (strcmp(buf,".block") == 0)
		{
			state = BLOCK;
			begin_block();
		}
		else if (strcmp(buf,".blocktitle") == 0)
		{
			state = BLOCK_TITLE;
			begin_block_title();
		}
		else if (strcmp(buf,".end") == 0)
		{
			if (state == BLOCK)
				end_block();
			state = 0;
		}
		else if (strcmp(buf,".eject") == 0)
		{
			eject();
			continue;
		}
		else switch (state)
		{
		case BLOCK:
			add_block (buf);
			break;
		case BLOCK_TITLE:
			add_block_title (buf);
			break;
		case DIVIDER:
			add_divider (buf);
			break;
		case TITLE:
			add_title (buf);
			break;
		default:
			output (buf);
			break;
		}
	}
}
