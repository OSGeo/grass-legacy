#define NAME	1
#define BOTTOM	2
#define TOP	3
#define LEFT	4
#define RIGHT	5

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"name", NAME,
		"right", RIGHT,
		"r", RIGHT,
		"left", LEFT,
		"l", LEFT,
		"top", TOP,
		"t", TOP,
		"bottom", BOTTOM,
		"bot", BOTTOM,
		"b", BOTTOM
	} ;
	static int n_variables = 10 ;
	float top ;
	float bottom ;
	float left ;
	float right ;
	char name[64] ;
#else
	extern float top ;
	extern float bottom ;
	extern float left ;
	extern float right ;
	extern char name[] ;
#endif
