#include "imagery.h"


static char title[80];
static char *vinfo[]=
{
    title,
    "",
    "Please select the group/subgroup containing the signatures",
    "to be used in the classification",
NULL };

main(argc,argv) char *argv[];
{
    char group[50];
    char subgroup[50];
    char sigfile[50];
    char name[50];
    char *prompt;
    char command[1024];

    G_gisinit (argv[0]);
    if (G_maskfd() >= 0)
    {
	printf ("\nWARNING: you have your mask set.\n");
	if (!G_yes("Do you want to continue? ", -1)) exit(0);
    }

    I_location_info (title, "MAXIMUM LIKELIHOOD");
    if(!I_vask_subgroup_old (vinfo, group, subgroup, 1, ""))
	exit(0);

    printf ("\nSIGNATURE");
    prompt = "Enter the signature file to be used for classification";
    if(!I_ask_signature_file_old (prompt, group, subgroup, sigfile))
	exit(0);


/* classified layer */
    if (!G_ask_cell_new (
	"Please name the CLASSIFIED map layer to be generated",name))
	    exit(0);

    sprintf (command, "i.maxlik gr='%s' sub='%s' sig='%s' cl='%s'",
	group, subgroup, sigfile, name);

/* reject layer - chi square tests */
    G_set_ask_return_msg ("if you don't want this layer");
    if (G_ask_cell_new (
	 "Please name the REJECT THRESHOLD map layer to be generated", name))
    {
	strcat (command, " rej='");
	strcat (command, name);
	strcat (command, "'");
    }
    exit(system(command));
}
