
#include "profile.h"
#include "stdio.h"

DumpProfile(profile)
struct Profile profile;
{
struct ProfileNode *ptr;

printf("\n\nWINDOW:\n");
printf("   north=%12.2lf\n",profile.window.north);
printf("   south=%12.2lf\n",profile.window.south);
printf("   east =%12.2lf\n",profile.window.east);
printf("   west =%12.2lf\n",profile.window.west);
printf("\nBEGINNING of profile-line:\n");
printf("   north1=%12.2lf\n",profile.n1);
printf("   east1 =%12.2lf\n",profile.e1);
printf("\nEND of profile-line:\n");
printf("   north2=%12.2lf\n",profile.n2);
printf("   east2 =%12.2lf\n",profile.e2);
printf("\nMAXIMUM CELL VALUE: %d\n",profile.MaxCat);
printf("\nMINIMUM CELL VALUE: %d\n",profile.MinCat);
printf("\nCELL VALUE COUNT  : %d\n",profile.count);

/*
printf("\nPROFILE VALUES\n");
ptr = profile.ptr;
while (ptr != NULL)
   {
   printf("%d\n",ptr->cat);
   ptr = ptr->next;
   }
*/
printf("\n");
}
