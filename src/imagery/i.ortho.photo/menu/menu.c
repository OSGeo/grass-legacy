#include <unistd.h>
/* menu.c */
#include "imagery.h"
#include "orthophoto.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
    char title[80];
    char buf[80];
    struct Ortho_Image_Group group;

    /* initialize grass */
    G_gisinit(argv[0]);

    /* get the current imagery group work with */
    if (!I_get_group(group.name)) { 
       if (!I_ask_group_old ("Enter imagery group for ortho-rectification", 
		       group.name))
	{
	 fprintf(stderr, "Use i.group to create a image group!\n");
         exit(0);
        }
    }

    /* get and check the group reference files */
    if (!I_get_group_ref (group.name, &group.group_ref))
    exit(1);
    if (group.group_ref.nfiles <= 0)
    {
        fprintf (stderr, "Group [%s] contains no files\n", group.name);
        sleep(3);
        exit(1);
    }
    
    while (1)
    {

        if (!I_get_group(group.name)) { 
           exit(0);
        }
	/* print the screenfull of options */ 
        sprintf (title, "i.ortho.photo -- \tImagery Group = %s ", group.name);
	G_clear_screen();

	fprintf (stderr, "%s\n\n", title);
	fprintf (stderr, "Initialization Options:\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "   1.     Select/Modify imagery group\n");
	fprintf (stderr, "   2.     Select/Modify imagery group target\n");
	fprintf (stderr, "   3.     Select/Modify target elevation model\n");
	fprintf (stderr, "   4.     Select/Modify imagery group camera\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "Transformation Parameter Computations:\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "   5.     Compute image-to-photo transformation\n");
	fprintf (stderr, "   6.     Initialize exposure station parameters\n");
	fprintf (stderr, "   7.     Compute ortho-rectification parameters\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "Ortho-rectification Option:\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "   8.     Ortho-rectify imagery files\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "RETURN   exit\n");
	fprintf (stderr, "\n> ");

	/* Get the option */
	if (!G_gets(buf))
	    continue;
	if (*buf == 0)    /* exit */
	    exit(0);

	/* run the program choosen */
	G_strip (buf);
	fprintf (stderr, "<%s>\n",buf);
	if (strcmp (buf, "1") == 0)
	    run_system ("i.group"); 
	if (strcmp (buf, "2") == 0)
	    run_etc_imagery ("photo.target", group.name); 
	if (strcmp (buf, "3") == 0)
	    run_etc_imagery ("photo.elev", group.name); 
	if (strcmp (buf, "4") == 0)
	    run_etc_imagery ("photo.camera", group.name);
	if (strcmp (buf, "5") == 0)
	    run_etc_imagery ("photo.2image", group.name); 
	if (strcmp (buf, "6") == 0)
	    run_etc_imagery ("photo.init", group.name); 
	if (strcmp (buf, "7") == 0)
	    run_etc_imagery ("photo.2target", group.name); 
	if (strcmp (buf, "8") == 0)
	    run_etc_imagery ("photo.rectify", group.name); 
    }
}

