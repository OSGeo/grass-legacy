#include "xgrass_lib.h"
#include "hourglass.h"
#include "hourglassmask.h"

typedef struct _bg_proc_list {
    int pid;
    char *etmp;
    char *stmp;
    char *buf;
    Widget shell;
    Boolean ignore;
    int *err;
    int nerr;
    struct _bg_proc_list *next;
} BgProcList;

#ifdef _NO_PROTO
static void RegBgProc();
static BgProcList * FindBgProc();
static void DeleteBgProc();
#else
static void RegBgProc( int pid, char *e, char *s, char *buf, Widget shell, int *err, int nerr);
static BgProcList * FindBgProc(int pid);
static void DeleteBgProc(int pid);
#endif

static BgProcList *procList;

DumpRegData()
{
   BgProcList *ptr = procList;
   while ( ptr) {
      fprintf(stderr,"Proc (%d,%s)\n", ptr->pid, ptr->buf);
      fflush(stderr);
      ptr = ptr->next;
       
      }
}

void
#ifdef _NO_PROTO
XgSystemMainLoop(appc)
XtAppContext appc;
#else
XgSystemMainLoop( XtAppContext appc)
#endif
{
    procList = NULL;
    while ( True ) {
	XEvent ev;

	if ( XtAppPending(appc) ) {
	    XtAppNextEvent(appc, &ev);
	    XtDispatchEvent(&ev);
	}
	if ( procList ) {
	    int pid;
	    BgProcList *ptr;
	    int status;
	    int deletepid = 0;

	    ptr = procList;
	    while ( ptr && !deletepid ) {
		if ( (pid = waitpid(ptr->pid,&status,WNOHANG)) == ptr->pid  ) {

		    /* Check for errors */
		    if ( WIFEXITED(status) != 0 ) {
			Boolean acceptable = False;
			int i;

			for  ( i = 0; i < ptr->nerr; i++ ) {
			     if ( WEXITSTATUS(status) == ptr->err[i] ) {
				 acceptable = True;
			     }
			}
			if ( !acceptable ) {
			    _XgHandleSystemError(ptr->shell, ptr->etmp);
			} else {
			    if ( !ptr->ignore ) {
				_XgHandleSystemOutput(ptr->shell, 
				    ptr->buf, ptr->stmp);
			    }
			}
			deletepid = pid;
		    }
		}
		if ( !deletepid )
		    ptr = ptr->next;
	    }
	    if ( deletepid ) {
		unlink(ptr->etmp);
		if ( !ptr->ignore )
		    unlink(ptr->stmp);
		DeleteBgProc(deletepid);
	    }
	}
    }
}

#ifdef _NO_PROTO
XgSystem(shell, buf, ignore, err, nerr)
Widget shell;
char *buf;
Boolean ignore;
int *err;
int nerr;
#else
XgSystem(Widget shell, char *buf, Boolean ignore, int *err, int nerr)
#endif
{
    char *errtmpl = "/tmp/xgerrtmpXXXXXX";
    char *sinktmpl = "/tmp/xgsinktmpXXXXXX";

    Display *display;
    Screen *screenPtr;
    Window window;
    Colormap cmap;
    int child, actual_child, w;
    int status;
    Boolean background = False;
    char *buf_copy;
    char *stmp_copy;
    char *etmp_copy;
    int  *err_copy;
    char *ampersand = NULL;

    Pixmap hourglassPixmap;
    Pixmap hourglassmaskPixmap;
    Cursor hourglassCursor;
    XColor blackColor, whiteColor, exact;
    Status resBlack, resWhite;
    char *etmp, *stmp;

    display = XtDisplay(shell);
    screenPtr = XtScreen(shell);
    window = XtWindow(shell);
    cmap = DefaultColormapOfScreen(screenPtr);

    resBlack = XAllocNamedColor(display, cmap, "black", &exact, &blackColor);
    resWhite = XAllocNamedColor(display, cmap, "white", &exact, &whiteColor);

    hourglassPixmap = XCreateBitmapFromData(display, window,
                   (char *)hourglass_bits, hourglass_width, hourglass_height);
    hourglassmaskPixmap = XCreateBitmapFromData(display, window,
                   (char *)hourglassmask_bits, hourglassmask_width, 
                    hourglassmask_height);
    hourglassCursor = XCreatePixmapCursor(display, hourglassPixmap,
                   hourglassmaskPixmap, &blackColor, &whiteColor,
                   hourglass_x_hot, hourglass_y_hot);

    XDefineCursor(display, window, hourglassCursor);
    XFlush(display);

    etmp = (char *)tmpnam(NULL);
    fflush(stderr);

    if ( ignore )
	stmp = "/dev/tty";
    else
	stmp = (char *)tmpnam(NULL);
    fflush(stdout);

    /* user is slipping a backgound command by us... */
    if ( (ampersand = (char *)strrchr(buf,'&')) != NULL ) {
	background = True;
	*ampersand = '\0';
    }
    buf_copy = XtNewString(buf);
    etmp_copy = XtNewString(etmp);
    stmp_copy = XtNewString(stmp);
    if ( nerr ) {
	int i;

	err_copy = (int *)XtCalloc(nerr, sizeof(int));
	for ( i = 0; i < nerr; i++ ) {
	    err_copy[i] = err[i];
	}
    } else {
	err_copy = NULL;
    }

    switch (child = fork()) {
        case -1: /* error */
            { 
                char errorbuf[1024];

		sprintf(errorbuf,"could not execute \"%s\"", buf_copy);
		XgWarningDialog(shell, errorbuf);
		XUndefineCursor(display, window);
		return;
            }
        case  0: /* child */
            {
                int tty;
		int flags = O_RDWR | O_CREAT;
		mode_t mode = S_IREAD | S_IWRITE;

                if ((tty = open(stmp_copy, flags, mode)) < 0 ) {
		    perror(stmp);
		    _exit(1);
                }
                if ( tty != 0 ) (void)dup2(tty, 1);

                if ((tty = open(etmp_copy, flags, mode)) < 0 ) {
		    perror(etmp);
		    _exit(1);
                }
                if ( tty != 0 ) (void)dup2(tty, 2);

                execl("/bin/sh","sh","-c",buf_copy,0);

                _exit(127);
            }
        default: /* parent */
            actual_child = child;
	    if ( background ) {

		RegBgProc(child, etmp_copy, stmp_copy, 
			  buf_copy, shell, ignore, err_copy, nerr);
	    }
            break;
    }

    if ( !background ) {
	while ( (w = waitpid(actual_child,&status,WNOHANG)) != actual_child && 
		 w != -1 ) ;

	/* Check for errors */
	if ( WIFEXITED(status) != 0 && WEXITSTATUS(status) != 0 ) {
	    Boolean acceptable = False;
	    int i;

	    for  ( i = 0; i < nerr; i++ ) {
		 if ( WEXITSTATUS(status) == err[i] ) {
		     acceptable = True;
		 }
	    }
	    if ( !acceptable ) {
		_XgHandleSystemError(shell, etmp);
		XUndefineCursor(display, window);
		unlink(etmp);
		unlink(stmp);
		return;
	    } else {
		if ( !ignore )
		_XgHandleSystemOutput(shell, buf_copy, stmp);
		unlink(etmp);
	    }
	} else {
	    if ( !ignore )
	    _XgHandleSystemOutput(shell, buf_copy, stmp);
	    unlink(etmp);
	}
	if ( !ignore )
	unlink(stmp);
    }  else {
	*ampersand = '&';
    }
    XUndefineCursor(display, window);
}

#ifdef _NO_PROTO
_XgHandleSystemOutput(w, buf, file)
Widget w;
char *buf;
char *file;
#else
_XgHandleSystemOutput(Widget w, char *buf, char *file)
#endif
{
    Arg al[10];
    int ac = 0;
    Widget editor;
    Widget dialog;
    struct stat sbuf;
    int fildes;
    int bytes;
    char *string;

    if ( !XtIsRealized(w)) {
	return;
    }
    if ( file == NULL || *file == NULL ) {
        return;
    }
    if ( stat(file,&sbuf) < 0 ) {
        return;
    }
    if ( sbuf.st_size == 0 ) {
        return;
    } 
    if ( ( fildes = open(file,O_RDONLY)) < 0 ) {
        return;
    }
    string = XtMalloc(sbuf.st_size + 1);
    if ( (bytes = read(fildes, string, sbuf.st_size)) == -1 ) {
        XtFree(string);
        return;
    }
    string[bytes] = '\0';

    dialog = XgEditor(w, string, "XGRASS Output Editor", buf, &editor);
    {
	Position x,y;
	Position newX,newY;
	Dimension width, height;
	Dimension dWidth, dHeight;

	XtVaGetValues(w, XmNx, &x, XmNy, &y, 
	    XmNwidth, &width, XmNheight, &height, NULL);
	XtVaGetValues(dialog, XmNwidth, &dWidth, XmNheight, &dHeight, NULL);

	newX = x + width/2 - dWidth/2;
	newY = y + height/2 - dHeight/2;

	XtMoveWidget(dialog,newX,newY);
    }
    XtPopup(dialog,XtGrabNone);
}

#ifdef _NO_PROTO
_XgHandleSystemError(w, file)
Widget w;
char *file;
#else
_XgHandleSystemError(Widget w, char *file)
#endif
{
    char buf[1024];
    struct stat sbuf;
    int fildes;
    int bytes;
    char *string;

    if ( !XtIsManaged(w)) {
	return;
    }
    if ( file == NULL || *file == NULL ) {
        return;
    }
    if ( stat(file,&sbuf) < 0 ) {
        return;
    }
    if ( sbuf.st_size == 0 ) {
        return;
    }
    if ( ( fildes = open(file,O_RDONLY)) < 0 ) {
        return;
    }
    string = XtMalloc(sbuf.st_size);
    if ( (bytes = read(fildes, string, sbuf.st_size)) == -1 ) {
        XtFree(string);
        return;
    }
    string[bytes] = '\0';

    XgError(w, string);
}

static void
#ifdef _NO_PROTO
RegBgProc(pid, e, s, buf, shell, ignore, err, nerr)
int pid;
char *e, *s;
char *buf;
Widget shell;
Boolean ignore;
int *err;
int nerr;
#else
RegBgProc( int pid, char *e, char *s, char *buf, 
	   Widget shell, Boolean ignore, int *err, int nerr)
#endif
{
    BgProcList *ptr = procList;

    if ( ptr == NULL ) {
        procList = ptr = (BgProcList *)XtMalloc(sizeof(BgProcList));
        bzero((char *)ptr, sizeof(BgProcList));
    } else {
	while ( ptr->next )  
            ptr = ptr->next;
        ptr->next = (BgProcList *)XtMalloc(sizeof(BgProcList));
	ptr = ptr->next;
        bzero((char *)ptr, sizeof(BgProcList));
    }
    ptr->pid = pid;
    ptr->etmp = e;
    ptr->stmp = s;
    ptr->buf = buf;
    ptr->shell = shell;
    ptr->ignore = ignore;
    ptr->err = err;
    ptr->nerr = nerr;
}

static BgProcList *
#ifdef _NO_PROTO
FindBgProc(pid)
int pid;
#else
FindBgProc(int pid)
#endif
{
    BgProcList *ptr = procList;

    while ( ptr ) {
        if ( pid == ptr->pid )
            return ptr;
        ptr = ptr->next;
    }
    return NULL;
}

static void
#ifdef _NO_PROTO
DeleteBgProc(pid)
int pid;
#else
DeleteBgProc(int pid)
#endif
{
    BgProcList *ptr = procList;
    BgProcList *tptr = procList;
    Boolean first = True;

    if ( pid == ptr->pid ) {
	ptr->shell = NULL;
	XtFree(ptr->buf);
	XtFree(ptr->etmp);
	XtFree(ptr->stmp);
	XtFree(ptr->err);
	if ( ptr->next == NULL ) {
	    procList = NULL;
	} else {
	    procList = ptr->next;
	    XtFree(ptr);
        }
	return;
    }
    while ( ptr->pid != pid ) {
        if ( first ) {
            first = False;
        } else {
	    tptr = tptr->next;
        }
	ptr = ptr->next;
    }
    tptr->next = ptr->next;
    ptr->shell = NULL;
    XtFree(ptr->buf);
    XtFree(ptr->etmp);
    XtFree(ptr->stmp);
    XtFree(ptr->err);
    XtFree(ptr);
}
