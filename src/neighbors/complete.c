/* %W% %G% */
#include "gis.h"
#include "ncb.h"
complete ()
{
    char command[100];
    FILE *mail;
    FILE *popen();

    sprintf (command,"mail '%s'", G_whoami());
    if (mail = popen(command,"w"))
    {
	fprintf(mail,"%s: %s complete\n", G_program_name(), ncb.newcell.name);
	pclose (mail);
    }
    else
	printf("\n%s: %s complete\n", G_program_name(), ncb.newcell.name);
}
