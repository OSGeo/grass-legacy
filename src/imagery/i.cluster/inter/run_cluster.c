#include "global.h"
#define CLUSTER "i.cluster"
#define VERB " -q"

run_cluster ()
{
  char command[1024];
  char tmp[256];

  sprintf (command, "%s  %s sub=%s sig=%s cla=%d ", CLUSTER, 
	   groupname, subgroupname, outsigname, class);

  if (!(verbose))
    strcat (command, VERB);

  if (sample_rows && sample_cols) {
    sprintf (tmp, " sam=%d,%d", sample_rows, sample_cols);
    strcat (command, tmp);
  }
  if (*seedname) {
    strcat (command, " see=");
    strcat (command, seedname);
  }
  if (*reportname) {
    strcat (command, " rep=");
    strcat (command, reportname);
  }

  sprintf (tmp," ite=%d con=%lf sep=%lf min=%d",iters,conv,sep,min_size);
  strcat (command, tmp);

  if (system(command))
    return 0;
  else
    return 1;
}
