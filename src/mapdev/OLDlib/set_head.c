/*
**  Written by Dave Gerdes 1/91
*/


#include "digit.h"
#include "dig_head.h"

/************************************************************************/
/*************** Low Level access to Head_array *************************/
/************************************************************************/

static struct dig_head *Head_array[MAX_OPEN_FILES];
static int  Head_Initialized = 0;

/* 
** User callable routine to get head structure for digit file
*/
struct dig_head *
dig_get_head (fp)
    FILE *fp;
{
    return dig__get_head (fp);
}

struct dig_head *
dig__get_head (fp)
    register FILE *fp;
{
    register int num;

    if (!Head_Initialized)
	G_fatal_error  ("Warning: dig__get_head called before init \n");

    num = fileno (fp);
    if (num < 0 || num >= MAX_OPEN_FILES)
    {
	/*DEBUG*/ fprintf (stderr, "DIG_FILENO error!\n");
	return  NULL;
    }
    return (Head_array[num]);	/* head*  or NULL */
}

dig__set_head (fp, dhead)
    FILE *fp;
    struct dig_head *dhead;
{
    register int num;
 
    if (!Head_Initialized)
	G_fatal_error  ("Warning: dig__set_head called before init \n");

    num = fileno(fp);
    if (num < 0 || num >= MAX_OPEN_FILES)
    {
	/*DEBUG*/ fprintf (stderr, "DIG_FILENO error 2!\n");
	return  -1;
    }
    Head_array[num] = dhead;
    return (num);
}

dig__get_head_num (fp)
    register FILE *fp;
{
    register int num;

    if (!Head_Initialized)
	G_fatal_error  ("Warning: dig__get_head_num called before init \n");

    num = fileno (fp);
    if (num <= 0 || num >= MAX_OPEN_FILES)
    {
	return  -1;
    }
    return num;
}

/* setup  head array MUST be called before any other head stuff! */
dig__initialize_VCB ()
{
    register int i;

    if (Head_Initialized)
	return 0;
    
    Head_Initialized = 1;
    for (i = 0 ; i < MAX_OPEN_FILES ; i++)
	Head_array[i] = NULL;

    return 1;
}
