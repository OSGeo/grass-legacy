/*************************************************************
* I_ask_camera_old (prompt,camera)
* I_ask_camera_new (prompt,camera)
* I_ask_camera_any (prompt,camera)
*
* prompt the user for an camera reference file name
*************************************************************/
#include "imagery.h"

I_ask_camera_old (prompt,camera)
    char *prompt;
    char *camera;
{
    while(1)
    {
	if (*prompt == 0)
	    prompt = "Select an camera reference file";
	if (!ask_camera(prompt, camera))
	    return 0;
	if (I_find_camera (camera))
	    return 1;
	printf ("\n** %s - not found **\n\n", camera);
    }
}

I_ask_camera_new (prompt, camera)
    char *prompt;
    char *camera;
{
    while(1)
    {
	if (*prompt == 0)
	    prompt = "Enter a new camera reference file name";
	if (!ask_camera(prompt, camera))
	    return 0;
	if (!I_find_camera (camera))
	    return 1;
	printf ("\n** %s - exists, select another name **\n\n", camera);
    }
}

I_ask_camera_any (prompt,camera)
    char *prompt;
    char *camera;
{
    if (*prompt == 0)
	prompt = "Enter a new or existing camera reference file";
    return ask_camera (prompt, camera);
}

static
ask_camera (prompt, camera)
    char *prompt;
    char *camera;
{
    char buf[1024];

    while (1)
    {
	printf ("\n%s\n", prompt);
	printf ("Enter 'list' for a list of existing camera files\n");
	printf ("Enter 'list -f' for a verbose listing\n");
	printf ("Hit RETURN %s\n", G_get_ask_return_msg());
	printf ("> ");
	if (!G_gets(buf)) continue;

	G_squeeze (buf);
	printf ("<%s>\n", buf);
	if (*buf == 0) return 0;

	if (strcmp (buf, "list") == 0)
	    I_list_cameras(0);
	else if (strcmp (buf, "list -f") == 0)
	    I_list_cameras(1);
	else if (G_legal_filename(buf) < 0)
	    printf ("\n** <%s> - illegal name **\n\n", buf);
	else   
	    break;
    }
    strcpy (camera, buf);
    return 1;
}
