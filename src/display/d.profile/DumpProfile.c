#include "profile.h"
#include "stdio.h"

int DumpProfile(struct Profile profile)
{
fprintf (stdout,"\n\nWINDOW:\n");
fprintf (stdout,"   north=%12.2f\n",profile.window.north);
fprintf (stdout,"   south=%12.2f\n",profile.window.south);
fprintf (stdout,"   east =%12.2f\n",profile.window.east);
fprintf (stdout,"   west =%12.2f\n",profile.window.west);
fprintf (stdout,"\nBEGINNING of profile-line:\n");
fprintf (stdout,"   north1=%12.2f\n",profile.n1);
fprintf (stdout,"   east1 =%12.2f\n",profile.e1);
fprintf (stdout,"\nEND of profile-line:\n");
fprintf (stdout,"   north2=%12.2f\n",profile.n2);
fprintf (stdout,"   east2 =%12.2f\n",profile.e2);
fprintf (stdout,"\nMAXIMUM CELL VALUE: %d\n",profile.MaxCat);
fprintf (stdout,"\nMINIMUM CELL VALUE: %d\n",profile.MinCat);
fprintf (stdout,"\nCELL VALUE COUNT  : %ld\n",profile.count);

/*
fprintf (stdout,"\nPROFILE VALUES\n");
ptr = profile.ptr;
while (ptr != NULL)
   {
   fprintf (stdout,"%d\n",ptr->cat);
   ptr = ptr->next;
   }
*/
fprintf (stdout,"\n");
   return 0;
}
