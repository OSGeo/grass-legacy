#include "globals.h"
#include "local_proto.h"

static int use = 1;
static int enlarge ;
static int shrink ;

static int replot(void);
static int determine_size(void);
static int newsize(void);
static int stop(void);
static int dont_stop(void);
static int really_quit(void);
static int pick(int,int);

int 
driver (void)
{
    static Objects objects[]=
    {
	MENU("QUIT",really_quit,&use),
	MENU("REPLOT",replot,&use),
	MENU("ENLARGE",newsize,&enlarge),
	MENU("SHRINK",newsize,&shrink),
	OTHER(pick, &use),
	{0}
    };

    determine_size();
    Input_pointer (objects);

    return 0;
}

static int 
replot (void)
{
    draw_image (VIEW_IMAGE, 0);
    return 0;
}

static int 
newsize (void)
{
    View *view;

    Menu_msg("");
    view = VIEW_IMAGE;
    VIEW_IMAGE = VIEW_IMAGE2;
    VIEW_IMAGE2 = view;
    determine_size();
    if (shrink)
	Erase_view (VIEW_IMAGE);
    else
	Erase_view (VIEW_IMAGE2);
    replot();
    return 0;
}

static int 
determine_size (void)
{
    if (VIEW_IMAGE->nrows > VIEW_IMAGE2->nrows)
    {
	shrink = 1;
	enlarge = 0;
    }
    else
    {
	shrink = 0;
	enlarge = 1;
    }
    return 0;
}

static int pick(int x,int y)
{
    if (change_color_assignment (x,y))
	return 0;
    return 0;
}

static int 
really_quit (void)
{
    int stop(), dont_stop();
    static Objects objects[] =
    {
	INFO("really quit? ",&use),
	MENU("NO",dont_stop,&use),
	MENU("YES",stop,&use),
	{0}
    };
    if (Input_pointer (objects) < 0)
	return -1;
    return 0; /* don't quit */
}

static int 
dont_stop (void)
{
    return 1;
}

static int 
stop (void)
{
    return -1;
}
