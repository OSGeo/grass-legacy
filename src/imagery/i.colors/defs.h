#include "imagery.h"

typedef struct
{
    int top, left, bottom, right;
} Box;

/* this is a graphics structure */
typedef struct
{
    int top, bottom ,left, right;
    int nrows, ncols;
    Box image;
    unsigned char *red, *grn, *blu;
    CELL *cell;
} View;


typedef struct
{
    char name[50];
    struct Ref ref;
} Group;

typedef struct
{
    int   type;         /* object type */
    int (*handler)();	/* routine to handle the event */
    char *label;	/* label to display if MENU or OPTION */
    int   binding;      /* OPTION bindings */
    int  *status;	/* MENU,OPTION status */
    int top,bottom,left,right;
} Objects;

#define MENU_OBJECT 1
#define OPTION_OBJECT 2
#define INFO_OBJECT 3
#define OTHER_OBJECT 4


#define MENU(label,handler,status) \
	{MENU_OBJECT,handler,label,0,status,0,0,0,0}
#define OPTION(label,binding,status) \
	{OPTION_OBJECT,NULL,label,binding,status,0,0,0,0}
#define INFO(label,status) \
	{INFO_OBJECT,NULL,label,0,status,0,0,0,0}
#define OTHER(handler,status) \
	{OTHER_OBJECT,handler,NULL,0,status,0,0,0,0}
