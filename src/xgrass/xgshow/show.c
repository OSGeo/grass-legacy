#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

Widget toplevel;
     
void open_file();
Widget text_w;
char *filename = NULL;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)NULL},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void 
#ifdef _NO_PROTO     
Dismiss(w, cld,cad)
Widget w;
XtPointer cld, cad;
#else
Dismiss( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    exit(0);
}
     
void
#ifdef _NO_PROTO     
main(argc, argv)
int argc;
char *argv[];
#else
main(int argc, char **argv)
#endif
{
  XtAppContext  app;
  Widget rc, button;
  Arg           args[10];
  XtTranslations tr;
  int n;
  
  toplevel = XtVaAppInitialize(&app, "Show", initTable, XtNumber(initTable),
			       &argc, argv, NULL, NULL);

  if (argc == 2){
    filename = (char *) malloc (strlen(argv[1]) + 1);
    strcpy(filename, argv[1]);
  } else {
    fprintf(stderr,"Usage: %s file\n",argv[0]);
  }
  
  rc = XmCreateRowColumn(toplevel, "rc", NULL, 0);

  n = 0;
  XtSetArg(args[n], XmNeditMode,  XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNeditable,  False); n++;
  text_w = XmCreateScrolledText(rc, "text_w", args, n);

  n = 0;
  XtSetArg(args[n], XmNlabelString, XmStringCreateSimple("Dismiss")); n++;
  button = XmCreatePushButton(rc,"button",args,n);
  XtAddCallback(button, XmNactivateCallback, Dismiss, NULL);

  XtManageChild(text_w);
  XtManageChild(button);
  XtManageChild(rc);
  if (filename)
    open_file(NULL, 0, NULL);
  
  XtRealizeWidget(toplevel);
  XtAppMainLoop(app);
}

void
#ifdef _NO_PROTO  
open_file(dialog, save, cbs)
Widget dialog;
int save;         /* actually, item_no from "new" or "save" */
XmFileSelectionBoxCallbackStruct *cbs;
#else
open_file(Widget dialog, int save, XmFileSelectionCallbackStruct *cbs);
#endif
{
    char buf[BUFSIZ], *text;
    struct stat statb;
    long len;
    FILE *fp;

    
    if ((cbs != NULL) && (!XmStringGetLtoR(cbs->value,
        XmSTRING_DEFAULT_CHARSET, &filename)))
        return; /* must have been an internal error */

    if (!*filename) {
        XtFree(filename);
        XBell(XtDisplay(text_w), 50);
        return; /* nothing typed */
    }

    if (save) {
        if (!(fp = fopen(filename, "w"))) {
            perror(filename);
            sprintf(buf, "Can't save to %s.", filename);
            XtFree(filename);
            return;
        }
        /* saving -- get text from Text widget... */
        text = XmTextGetString(text_w);
        len = XmTextGetLastPosition(text_w);
        /* write it to file (check for error) */
        if (fwrite(text, sizeof(char), len, fp) != len)
            strcpy(buf, "Warning: did not write entire file!");
        else {
            /* make sure a newline terminates file */
            if (text[len-1] != '\n')
                fputc(fp, '\n');
            sprintf(buf, "Saved %ld bytes to %s.", len, filename);
        }
    } else {
        /* make sure the file is a regular text file and open it */
        if (stat(filename, &statb) == -1 ||
                (statb.st_mode & S_IFMT) != S_IFREG ||
                !(fp = fopen(filename, "r"))) {
            perror(filename);
            sprintf(buf, "Can't read %s.", filename);
            XtFree(filename);
            return;
        }
        /* put the contents of the file in the Text widget by
         * allocating enough space for the entire file, reading the
         * file into the space, and using XmTextSetString() to show
         * the file.
         */
        len = statb.st_size;
        if (!(text = XtMalloc((unsigned)(len+1)))) /* +1 for NULL */
            sprintf(buf, "%s: XtMalloc(%ld) failed", len, filename);
        else {
            if (fread(text, sizeof(char), len, fp) != len)
                sprintf(buf, "Warning: did not read entire file!");
            else
                sprintf(buf, "Loaded %ld bytes from %s.",
                    len, filename);
            text[len] = 0; /* NULL-terminate */
            XmTextSetString(text_w, text);
        }
    }

    /* free all allocated space. */
    XtFree(text);
    fclose(fp);
}
