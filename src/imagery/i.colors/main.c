#define GLOBAL
#include <unistd.h>
#include <signal.h>
#include "raster.h"
#include "globals.h"
#include "local_proto.h"

#ifdef __GNUC_MINOR
static void quit (int) __attribute__ ((__noreturn__));
#else
static void quit (int);
#endif
static int error(char *,int);

int main (int argc, char *argv[])
{
    G_gisinit (argv[0]);
    G_get_window (&window);

    interrupt_char = G_intr_char();
    tempfile1 = G_tempfile();
    tempfile2 = G_tempfile();

    R_open_driver();

    if (!I_ask_group_old ("", group.name))
	exit(0);
    if (!I_get_group_ref (group.name, &group.ref))
	exit(1);
    if (group.ref.nfiles <= 0)
    {
	fprintf (stderr, "Group [%s] contains no files\n", group.name);
	sleep(3);
	exit(1);
    }
    I_read_group_colors (group.name, &group.ref);

    signal (SIGINT, quit);

    Init_graphics();

    G_set_error_routine (error);

    allocate_color_bars();
    display_title();
    display_color_assignment();
    draw_image(VIEW_IMAGE, 0);


/* go do the work */
    driver();

    quit(0);
}

static void quit (int n)
{
    R_close_driver();
    unlink (tempfile1);
    unlink (tempfile2);
    exit(n);
}

static int error(char *msg,int fatal)
{
    char buf[200];
    int x,y,button;

    Beep();
    if (fatal)
	sprintf (buf, "ERROR: %s", msg);
    else
	sprintf (buf, "WARNING: %s (click mouse to continue)", msg);
    Menu_msg (buf);

    if (fatal)
	quit(1);
    Mouse_pointer (&x, &y, &button);

    return 0;
}

int Beep (void)
{
    write (1,"\7",1);

    return 0;
}
