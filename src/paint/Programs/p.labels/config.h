#define TEXTLINES 4
struct config
{
	char text[TEXTLINES][50];
	char skip[5];
	char north[20];
	char east[20];
	char color[20];
	char hcolor[20];
	char background[20];
	char border[20];
	char ref[20];
	char xoffset[10];
	char yoffset[10];
	char size[10];
	char width[10];
	char hwidth[10];
	char opaque[10];
	char font[20];
	int count;
} ;
#ifdef MAIN
	struct config config ;
#else
	extern struct config config ;
#endif

#define TEXT 3
#define EAST (TEXT+TEXTLINES+1)
#define NORTH (EAST+1)
#define REF (NORTH+1)
#define FONT (REF+2)
#define SIZE (FONT+1)
#define COLOR (SIZE+1)
#define HCOLOR (COLOR+1)
#define BACKGROUND (HCOLOR+2)
#define BORDER (BACKGROUND+1)
#define OPAQUE (BORDER+1)
