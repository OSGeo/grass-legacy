#include "xgrass_lib.h"
#include "hourglass.h"

#if defined(SUNOS) || defined(BSD) || defined(mips)
#define PIDTYPE int
#else
#define PIDTYPE pid_t
#endif

typedef struct _bg_proc_list {
    PIDTYPE pid;
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
static void RegBgProc( PIDTYPE pid, char *e, char *s, char *buf, Widget shell, Boolean ignore, int *err, int nerr);
static BgProcList * FindBgProc(PIDTYPE pid);
static void DeleteBgProc(PIDTYPE pid);
#endif

static BgProcList *procList = NULL;
static XtAppContext appContext;

#ifdef _NO_PROTO
XgMainLoop(ac)
    XtAppContext ac;
#else
XgMainLoop(XtAppContext ac)
#endif
{
    appContext = ac;
    XtAppMainLoop(ac);
}

DumpRegData()
{
   BgProcList *ptr = procList;
   while ( ptr) {
      fprintf(stderr,"Proc (%d,%s)\n", ptr->pid, ptr->buf);
      fflush(stderr);
      ptr = ptr->next;
       
      }
}

XtTimerCallbackProc
#ifdef _NO_PROTO
DoFinishCommand(client_data, id)
    XtPointer client_data; 
    XtIntervalId *id;
#else
DoFinishCommand(XtPointer client_data, XtIntervalId *id)
#endif
{
    if ( procList ) {
	PIDTYPE pid;
	BgProcList *ptr;
#ifdef mips
	union wait status;
#else
	int status;
#endif
	PIDTYPE deletepid = 0;

	ptr = procList;
	while ( ptr && !deletepid ) {
#ifdef mips
	    if ( (pid = wait3(&status,WNOHANG, NULL)) == ptr->pid  ) {
#else
	    if ( (pid = waitpid(ptr->pid,&status,WNOHANG)) == ptr->pid  ) {
#endif

		/* Check for errors */
		if ( WIFEXITED(status) != 0 ) {
		    Boolean acceptable = False;
		    int i;

		    for  ( i = 0; i < ptr->nerr; i++ ) {
#ifdef mips
			 if ( (status).w_retcode == ptr->err[i] ) {
#else
			 if ( WEXITSTATUS(status) == ptr->err[i] ) {
#endif
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

int
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

    int child, actual_child, w;
#ifdef mips
    union wait status;
#else
    int status;
#endif
    Boolean background = False;
    char *buf_copy;
    char *stmp_copy;
    char *etmp_copy;
    int  *err_copy;
    char *ampersand = NULL;
#ifdef BSD
    int KidDied();
#else
    void KidDied();
#endif

    Window window;
    char *etmp, *stmp;

    XgDoHourGlass(shell);

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

    if ( background ) (void) signal(SIGCHLD,KidDied);

    switch (child = fork()) {
        case -1: /* error */
            { 
                char errorbuf[1024];

		sprintf(errorbuf,"could not execute \"%s\"", buf_copy);
		XgWarningDialog(shell, errorbuf);
		XgUndoHourGlass(shell);
		return 0;
            }
        case  0: /* child */
            {
                int tty;
		int flags = O_RDWR | O_CREAT;
#ifdef mips
		int mode = S_IREAD | S_IWRITE;
#else
		mode_t mode = S_IREAD | S_IWRITE;
#endif

                if ((tty = open(stmp_copy, flags, mode)) < 0 ) {
fprintf(stderr,"STMP = %s\n", stmp_copy);
		    perror(stmp);
		    exit(1);
                }
                if ( tty != 0 ) (void)dup2(tty, 1);

                if ((tty = open(etmp_copy, flags, mode)) < 0 ) {
fprintf(stderr,"ETMP = %s\n", etmp_copy);
		    perror(etmp);
		    exit(1);
                }
                if ( tty != 0 ) (void)dup2(tty, 2);

                execl("/bin/sh","sh","-c",buf_copy,0);

                exit(127);
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
#ifdef mips
	while ( (w = wait3(&status,WNOHANG, NULL)) != actual_child && 
		 w != -1 ) ;
#else
	while ( (w = waitpid(actual_child,&status,WNOHANG)) != actual_child && 
		 w != -1 ) ;
#endif

	/* Check for errors */
#ifdef mips
	if ( WIFEXITED(status) != 0 && (status).w_retcode != 0 ) {
#else
	if ( WIFEXITED(status) != 0 && WEXITSTATUS(status) != 0 ) {
#endif
	    Boolean acceptable = False;
	    int i;

	    for  ( i = 0; i < nerr; i++ ) {
#ifdef mips
		 if ( status.w_retcode == err[i] ) {
#else
		 if ( WEXITSTATUS(status) == err[i] ) {
#endif
		     acceptable = True;
		 }
	    }
	    if ( !acceptable ) {
		_XgHandleSystemError(shell, etmp);
		XgUndoHourGlass(shell);
		unlink(etmp);
		unlink(stmp);
		return 0;
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
    XgUndoHourGlass(shell);
    return child;
}

#ifdef BSD
int
#else
void
#endif
KidDied()
{
    XtAppAddTimeOut(appContext, 0, DoFinishCommand, NULL);
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

    if ( !XtIsManaged(w) && ! XtIsRealized(w)) {
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
PIDTYPE pid;
char *e, *s;
char *buf;
Widget shell;
Boolean ignore;
int *err;
int nerr;
#else
RegBgProc( PIDTYPE pid, char *e, char *s, char *buf, 
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
PIDTYPE pid;
#else
FindBgProc(PIDTYPE pid)
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
PIDTYPE pid;
#else
DeleteBgProc(PIDTYPE pid)
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
