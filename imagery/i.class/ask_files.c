#include <stdlib.h>
#include "gis.h"
#include "glocale.h"
#include "globals.h"
#include "local_proto.h"


char title[80];

static char *info[]=
{
title,
"",
"Please select the group/subgroup to be analyzed",
NULL
};


int ask_files (char *me)
{
    char *name, *mapset;
    FILE *fd;
    int n, any;
    int hitreturn;

    I_location_info (title, "SUPERVISED CLASSIFIER");
    hitreturn = 0;
    I_init_group_ref (&Refer);
    while(1)
    {
	if (hitreturn)
	{
	    char buf[100];
	    G_message(_("Hit RETURN -->"));
	    G_gets(buf);
	}
	hitreturn = 1;
	if (!I_vask_subgroup_old (info, Group, Subgroup, 1, ""))
	    exit(0);
	I_free_group_ref (&Refer);
	I_get_subgroup_ref (Group, Subgroup, &Refer);

	any = 0;
	for (n = 0; n < Refer.nfiles; n++)
	{
	    name = Refer.file[n].name;
	    mapset = Refer.file[n].mapset;
	    if (G_find_cell (name, mapset) == NULL)
	    {
		if (!any)
		    G_warning(_("\7\n** The following cell files in subgroup "
                              "[%s] do not exist."), Subgroup);
		any = 1;
		G_message(_("       %s@%s"), name, mapset);
	    }
	}
	if (any) continue;
	if (Refer.nfiles > 1)
	    break;
	G_message(_("Subgroup [%s] "), Subgroup);
	if (Refer.nfiles <= 0)
	    G_message(_("doesn't have any files."));
	else
	    G_message(_("only has 1 file."));
	G_warning(_("The subgroup must have at least 2 files to run %s."), me);
    }

    if (G_get_cellhd(Refer.file[0].name,Refer.file[0].mapset,&Band_cellhd)!=0)
      G_fatal_error(_("Unable to read cell header for first band file."));

/* allocate space for signature routines */
    init_sig_routines(Refer.nfiles);

    G_message(_("\nRESULT SIGNATURE"));
    if(!I_ask_signature_file_any (_("Enter name for the resulting signature file"),
            Group, Subgroup,  Outsigfile))
	exit(0);
    Outsigfile_fd = I_fopen_signature_file_new(Group, Subgroup, Outsigfile);
    if(Outsigfile_fd == NULL)
      G_fatal_error(_("Unable to open output signature file."));

    for(;;)
    {
	I_init_signatures (&Sigs, Refer.nfiles);
	G_set_ask_return_msg ("to not include any other signatures");
	G_message(_("\nSEED SIGNATURES"));
	if (!I_ask_signature_file_old(
	    _("Select the signature file to include in the resulting file"),
	    Group, Subgroup, Insigfile))
	{
	    Insigfile[0] = 0;
	    break;
	}
	fd = I_fopen_signature_file_old (Group, Subgroup, Insigfile);
	if (fd == NULL) continue;
	n = I_read_signatures (fd, &Sigs);
	fclose (fd);
	if (n < 0)
	{
	    G_warning(_("** Can't read signature file [%s] **"), Insigfile);
	    continue;
	}
	if (Sigs.nsigs <= 255) break;
	G_warning(_("%s has too many signatures."), Insigfile);
	I_free_signatures(&Sigs);
    }

    return 0;
}
