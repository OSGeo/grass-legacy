/* %W% %G% */
#include "menu.h"
/*=====================================================================
 |		This program is a simple example of the use of the
 |	package of routines found in menu.c. Compilation procedures
 |	and requirements can be found in the accompanying Makefile.
 */

main (argc, argv)
int	argc;
char	**argv;
{
int	Dummy, Flags, Scum, Mutant, Length;
char	Out_buf[256], *Tablbuf;


	Scum = F_fetchfile ("dumtable", &Tablbuf, &Length);
	P_writowin (TablW, Tablbuf, 1, Length, HELPWINHITE - 2);
	if (Scum < 0)
		P_menuerror (Scum, "dumtable");
/*
	Scum = F_menu (argv[1], argv[2], &Dummy, Out_buf, &Flags);	
	Scum = F_helpctrl ("basshelp");	
*/

	P_menuexit ();
	P_termexit ();
	fprintf (stderr, "%d\n%d\n%s\n", Scum, Dummy, Out_buf);
}
