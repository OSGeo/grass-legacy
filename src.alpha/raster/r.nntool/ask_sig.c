#include "globals.h"
char title[80];
static char *info[]= {
title,
"",
"Please select the group/subgroup to be analyzed",
NULL
};

ask_sig()
{
extern char *lname[10], *Gmapset[10];
extern int numfiles;
FILE *fd;
int n, any;
char *mapset, *name;

    I_init_group_ref (&Refer);
    while(1) {
	if(!I_vask_subgroup_old (info, Group, Subgroup, 1, ""))
		exit(0);
	I_free_group_ref (&Refer);
	I_get_subgroup_ref (Group, Subgroup, &Refer);

	any = 0;
	for(n=0;n < Refer.nfiles;n++) {
		name = Refer.file[n].name;
		mapset = Refer.file[n].mapset;
		if (G_find_cell (name, mapset) == NULL) {
			if(!any)
				printf("\7\n** The following cell files in subgroup [%s] do not exist\n", Subgroup);
			any = 1;
			printf("       %s@%s\n", name, mapset);
		}
	}
	if(any) continue;
	if(Refer.nfiles > 1)
		break;
	printf("Subgroup [%s] ", Subgroup);
	if(Refer.nfiles <= 0)
		printf("doesn't have any files\n");
	else
		printf("only has 1 file=n");
	printf("The subgroup must have at least 2 files to run\n");
    }
    if(G_get_cellhd(lname[0], Gmapset[0], &Band_cellhd)!=0)
	G_fatal_error("Unable to read cell header for first band file.");

/* allocate space for signature routines */
    init_sig_routines(numfiles);

    printf("\nRESULT SIGNATURE");
    if(!I_ask_signature_file_any ("Enter name for the resulting signature file",
  Group, Subgroup, Outsigfile))
	exit(0);
    Outsigfile_fd = I_fopen_signature_file_new(Group, Subgroup, Outsigfile);
    if(Outsigfile_fd == NULL)
	G_fatal_error("Unable to open output signature file");

    for(;;)
    {
	I_init_signatures (&Sigs, Refer.nfiles);
	G_set_ask_return_msg("to not include any other signatures");
	printf("\nSEED SIGNATURES");
	if(!I_ask_signature_file_old(
		"Select the signature file to include in the resulting file",
		Group, Subgroup, Insigfile))
	{
		Insigfile[0] = 0;
		break;
	}
	fd = I_fopen_signature_file_old (Group, Subgroup, Insigfile);
	if (fd == NULL) continue;
	n = I_read_signatures (fd, &Sigs); 
	fclose(fd);
	if(n < 0)
	{
		printf("** can't read signature file %s **\n", Insigfile);
		continue;
	}
	if (Sigs.nsigs <= 255) break;
	printf("%s has too many signatures\n", Insigfile);
	I_free_signatures(&Sigs);
     }

/*    G_get_cellhd(lname[0], Gmapset[0], &Band_cellhd);
    init_sig_routines(numfiles);
    printf ("\nRESULT SIGNATURE");
    if(!I_ask_signature_file_any ("Enter name for the resulting signature file", Group, Subgroup,  Outsigfile))
        exit(0);
    Outsigfile_fd = I_fopen_signature_file_new(Group, Subgroup, Outsigfile);
    if(Outsigfile_fd == NULL)
      G_fatal_error("Unable to open output signature file"); */
}
