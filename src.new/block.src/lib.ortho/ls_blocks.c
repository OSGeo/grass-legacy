/*************************************************************
* I_list_blocks (full)
* I_list_subblocks (block, full)
*************************************************************/
#include "dba_imagery.h"

static char *tempfile = NULL;

I_list_blocks (full)
{
    char *element;
    int i;

    char buf[1024];
    char title[50];
    FILE *ls, *temp, *popen();
    struct Block_Image_Group_Ref Block_Image_Group_Ref;
    int any;

    if (tempfile == NULL)
	tempfile = G_tempfile();

    element = "block";
    G__make_mapset_element (element);

    temp = fopen (tempfile, "w");
    if (temp == NULL)
	G_fatal_error ("can't open any temp files");
    fprintf (temp, "Available blocks\n");
    fprintf (temp, "---------------------------------\n");

    any = 0;
    strcpy (buf, "cd ");
    G__file_name (buf+strlen(buf), element, "", G_mapset());
    strcat (buf, ";ls");
    if (!full) strcat (buf, " -C");
    if(ls = popen (buf, "r"))
    {
	while (G_getl(buf, sizeof buf, ls))
	{
	    any=1;
	    fprintf (temp, "%s", buf);
	    if (full)
	    {
		I_get_block_title (buf, title, sizeof title);
		if (*title)
		    fprintf (temp, " (%s)", title);
		fprintf (temp, "\n");
		I_get_block_Block_Image_Group_Ref (buf, &Block_Image_Group_Ref);
		for (i = 0; i < Block_Image_Group_Ref.nfiles; i++)
		    fprintf (temp, "\t%s in %s\n", Block_Image_Group_Ref.file[i].name, Block_Image_Group_Ref.file[i].mapset);
		if (Block_Image_Group_Ref.nfiles <= 0)
		    fprintf (temp, "\t** empty **\n");
		I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref);
	    }
	    else 
		fprintf (temp, "\n");
	}
	pclose (ls);
    }
    if (!any)
	fprintf (temp, "no block files available\n");
    fprintf (temp, "---------------------------------\n");
    fclose (temp);
    sprintf (buf, "more -d %s", tempfile);
    system(buf);
    unlink (tempfile);
    printf ("hit RETURN to continue -->");
    G_gets(buf);
}

I_list_subblocks (block, full)
    char *block;
{
    char element[100];
    int i;

    char buf[1024];
    FILE *ls, *temp, *popen();
    struct Block_Image_Group_Ref Block_Image_Group_Ref;
    int any;

    if (tempfile == NULL)
	tempfile = G_tempfile();

    sprintf (element, "block/%s/subblock", block);
    G__make_mapset_element (element);

    temp = fopen (tempfile, "w");
    if (temp == NULL)
	G_fatal_error ("can't open any temp files");
    fprintf (temp, "Available subblocks in block %s\n", block);
    fprintf (temp, "---------------------------------\n");

    any = 0;
    strcpy (buf, "cd ");
    G__file_name (buf+strlen(buf), element, "", G_mapset());
    strcat (buf, ";ls");
    if (!full) strcat (buf, " -C");
    if(ls = popen (buf, "r"))
    {
	while (G_getl(buf, sizeof buf, ls))
	{
	    any=1;
	    fprintf (temp, "%s\n", buf);
	    if (full)
	    {
		I_get_subblock_Block_Image_Group_Ref (block, buf, &Block_Image_Group_Ref);
		for (i = 0; i < Block_Image_Group_Ref.nfiles; i++)
		    fprintf (temp, "\t%s in %s\n", Block_Image_Group_Ref.file[i].name, Block_Image_Group_Ref.file[i].mapset);
		if (Block_Image_Group_Ref.nfiles <= 0)
		    fprintf (temp, "\t** empty **\n");
		I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref);
	    }
	}
	pclose (ls);
    }
    if (!any)
	fprintf (temp, "no subblock files available\nRun i.build.block to create sub-blocks for block %s.\n", block);
    fprintf (temp, "\n---------------------------------\n");
    fclose (temp);
    sprintf (buf, "more -d %s", tempfile);
    system(buf);
    unlink (tempfile);
    printf ("hit RETURN to continue -->");
    G_gets(buf);
}
