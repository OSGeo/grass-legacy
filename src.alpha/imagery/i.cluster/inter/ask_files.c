#include "global.h"

ask_files ()
{
  char *prompt;
  char buf[100];

  verbose = 0;

  G_clear_screen();

  fprintf (stderr,"You may run i.cluster to generate signature file only,\n");
  fprintf (stderr,"or accompanied by a report file in a specified directory\n");
  fprintf (stderr,"(report-filename-only results in current directory).\n");

  prompt = "Enter the group filename";
  if (!I_ask_group_old (prompt, groupname))
    exit (1);
  
  prompt = "Enter the subgroup filename";
  if (!I_ask_subgroup_old (prompt, groupname, subgroupname))
    exit (1);

  fprintf (stderr, "\nRESULT SIGNATURE");
  if(!I_ask_signature_file_any ("Enter name for the resulting signature file",
      groupname, subgroupname, outsigname))
    exit(1);

  G_set_ask_return_msg ("to use DEFAULT means");
  fprintf (stderr, "\nSEED SIGNATURES");
  if (!I_ask_signature_file_old(
      "Select the signature file to use for the initial class means",
       groupname, subgroupname, seedname))
    seedname[0] = 0;

  fprintf (stderr,"\nThe report file contains current location, mapset, region \n");
  fprintf (stderr,"coordinates, imagery group name, subgroup name and mask information. \n");
  fprintf (stderr,"Initial parameters of clustering, means and standard deviations of \n");
  fprintf (stderr,"each band, and initial class means and standard deviations for each \n");
  fprintf (stderr,"band are included.\n");
  fprintf (stderr,"\n");
  fprintf (stderr,"At each step of iteration, number of classes, percentage \n");
  fprintf (stderr,"of stable points, and points of each class are recorded. \n");
  fprintf (stderr,"Finally, results including number of classes, class \n");
  fprintf (stderr,"separability matrix, and class means and standard deviations \n");
  fprintf (stderr,"for each band are at the end of the report file.\n"); 
  
  while (G_yes ("\nWould you like to have a report file? ", 1)) {
    do 
      fprintf (stderr, "Enter report filename: ");
    while (!G_gets (reportname));
    if (*reportname == 0) continue;
    if (access (reportname, 0) == 0) {
      sprintf (buf, "<%s> exists. OK to overwrite? ", reportname);
      if (G_yes(buf,1))
	break;
    }
    else
      break;
  }

  if (!G_yes ("\nWould you like to run in the background? ", 0)) {
	verbose++;
  }
}
