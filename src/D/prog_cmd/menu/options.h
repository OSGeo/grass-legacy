/*  %W%   %G%  */

#define BCOLOR	1
#define TCOLOR	2
#define DCOLOR	3
#define SIZE	4

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
	    "size", SIZE,
	    "s", SIZE,
	    "textcolor", TCOLOR,
	    "t", TCOLOR,
	    "backcolor", BCOLOR,
	    "b", BCOLOR,
	    "dividercolor", DCOLOR,
	    "d", DCOLOR
	} ;
	static int n_variables = 8 ;

	int backcolor ;
	int textcolor ;
	int dividercolor ;
	int size ;
	int left ;
	int top ;
	char *options[100] ;
#else
	extern int backcolor ;
	extern int textcolor ;
	extern int dividercolor ;
	extern int size ;
	extern int left ;
	extern int top ;
#endif
