#include "text.h"

#ifdef MAIN

	TEXT_LIST title_text;
	TEXT_LIST divider_text;
	TEXT_LIST block_text;
	TEXT_LIST block_title_text;
	int lineno;
	int pageno;
	int pagelen;

#else

	extern TEXT_LIST title_text;
	extern TEXT_LIST divider_text;
	extern TEXT_LIST block_text;
	extern TEXT_LIST block_title_text;
	extern int lineno ;
	extern int pageno ;
	extern int pagelen ;

#endif
