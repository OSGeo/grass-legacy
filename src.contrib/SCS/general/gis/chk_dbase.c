#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

can_make_location (gisdbase, location)
    char *gisdbase;
    char *location;
{
    struct stat s;
    struct passwd *pwd, *getpwuid();

/* make sure this is a directory */
    if (stat (gisdbase, &s) != 0)
    {
	printf ("\n** %s not found **\n", gisdbase);
	return 0;
    }
    if (!(s.st_mode & S_IFDIR))
    {
	printf ("\n** %s is not a directory **\n", gisdbase);
	return 0;
    }

/* look for write permission */
    if (access (gisdbase, 2) == 0)
	return 1;
    
    printf ("\nNote\n");
    printf (" You don't have permission under %s to create a new location\n",
	gisdbase);
    if (pwd = getpwuid (s.st_uid))
	printf (" See user %s about creating location %s\n", pwd->pw_name, location);
    return 0;
}
