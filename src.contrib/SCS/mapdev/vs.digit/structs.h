
struct Menu_item
{
    char *text;
    char command;
    char enabled;
};

struct Menu_head
{
    struct Menu_item *item;
    char *name;
    char changed;
    int (*help)() ;
};

struct windows
{
	char *name ;
	float bot, top, left, right ;
} ;

#ifdef MAIN
struct windows windows[] =
	{
	"dig", 0, 100,   0,  60,
	"men", 0, 100,   60, 100
	} ;
#else
extern struct windows windows[] ;
#endif

#define DIG	windows[0]
#define MEN	windows[1]
