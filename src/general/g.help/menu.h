#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <curses.h>
#include <ctype.h>
#include <math.h>
/*==================================================================
 |		This file contains the Constants, Structures, and
 |	Variables referenced by the menu program for the CERL Fort
 |	Hood Information System Menu Driver. These symbols are used 
 |	by the program living in file mentst.c.
 */

#define NCOLS        80
#define NOERR_ERRNUM  0
#define FILSTA_ERRNUM 1
#define FILOPN_ERRNUM 2
#define FILRED_ERRNUM 3
#define	FILCLS_ERRNUM 4
#define	NOMEM_ERRNUM  5
#define FRKERR_ERRNUM 6
#define	IVLDCD_ERRNUM 7
 
static int MENWINHITE ;
static int HELPWINHITE ;
#define TABLWINHITE   6
#define CMDWINHITE   7
#define MAXSHRTRESP   70
#define COMMANDFILE   "Mencommands"
#define TMPFILE       "./tempo"

#define NULLSTR "\0\n"   
#define	LF	'\n'
#define	CR	'\012'
#define	ESC	'\033'
#define	VERTCHAR	' '
#define	HORZCHAR	'-'
#define	BKSPC	'\b'
#define	BLANK	' '
#define	KILLINE	 '\027'			/* Ctrl w */
#define TOPPAG   '\024'			/*      t */
#define BOTPAG   '\005'			/*      e */
#define BAKPAG   '\002'			/*      b */
#define FORPAG   '\006'			/*      f */
#define BAKLIN   '\004'			/*      d */
#define FORLIN   '\025'			/*      u */
#define REFRESH  'r'
#define PRINTOUT '\001' 		/* Ctrl a */ 
#define HELP   	 '?'  
#define PAGEUP       '\065'
#define PAGEDOWN     '\066'
#define ARROWUP       'u' /*u bug: what's correct number? */
#define ARROWDOWN     'd' /* bug: what's correct number? */

#define MICRORESP	1
#define MICROMESG	"Immediate response"
#define MINIRESP	2
#define MINIMESG	"Enter keyword"
#define LINERESP	4
#define LINEMESG	"One line response"
#define LONGRESP	8
#define LONGMESG	"File edit response"
#define CHOOSERESP	16
#define CHOOSEMESG	"Choose response"
#define DEFLTRESP	MINIRESP
#define DEFLTMESG	MINIMESG

#define LEFTMARG	1
 
/*---------------------------------------------
 |		Global Data Structures
 */

WINDOW	*MenuW, *PlanetW, *PrmptW, *TablW;
WINDOW	*StatehelpW, *CommandhelpW, *ErrorW;


int F_menu(char *, char *, int *, char *, int *, int *, int *);
int P_writowin(WINDOW *, char *, int, int, int);
int P_writo_Menu_win(WINDOW *, char *, int, int, int, int *, char *);
void P_termexit(int);
int F_linecnt(FILE *);
int P_menuinit(void);
int P_menuexit(void);
char *F_universe(char *);
int F_helpctrl(char *);
int P_getusrinpt(char *, char *);
int F_longresponse(void);
int F_fetchfile(char *, char **, int *);
int P_menuerror(int, char *);
int dump_window(void);
