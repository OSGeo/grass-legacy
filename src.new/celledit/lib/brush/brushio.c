#include <stdio.h>
#include <math.h>
#include <sys/file.h>
#include "../brush.h"
#include "../misc.h"

# define ONELINE 81

static char *brushKeys[] =  {
    "WIDTH",
    "HEIGHT",
    "VALUES",
    "FUNCS",	
    };

# define ERROR -1
# define WIDTH 0
# define HEIGHT 1
# define VALUES 2
# define FUNCS 3

/***********************************************************************

File     	:	brushio.c
Function 	:	ReadBrush(fName, brush)
Args	 	:	    char *fName; -- absolute path to brush
  	    		    B_PTR brush; -- structure to fill.

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	19 March 1990
Last Revised	:
Abstract 	:	Reads a brushfile into the brush structure
Returns  	:	1 if successful, 0 if not.

***********************************************************************/
ReadBrush(fName, brush)
    char *fName; /* full path name to the brush */
    B_PTR brush;
    {
    FILE *brushFile = (FILE *)OpenInfile(fName);
    int	lineNum = 0;
    int readin;
    char	errStr  [ONELINE];
    char	keyWord [ONELINE];
    char	oneLine [ONELINE];


    if(brushFile == (FILE *)NULL)
	return (0);
    readin = 1;
    strcpy(brush->name, (char *)GetFileName(fName));
    while (!feof(brushFile))
	{
	if (readin)
	    {
	    lineNum++;
	    fgets (oneLine, 80, brushFile);
	    StripBlanks(oneLine);
	    if ((*oneLine == '#') || 	        /*  a comment line  */
		    (*oneLine == NULL))		/*  an empty line  */
		    continue;
	    }
	readin = 1;
	GetWord (oneLine, keyWord);
	StrToUpper(keyWord);
	switch (MatchKeyword (keyWord, brushKeys))
	    {
	    case ERROR:
		{
		sprintf (errStr, "line %d: unknown keyword '%s'.",
				  lineNum, keyWord);
		fprintf(stderr, errStr);
		fclose (brushFile);
		return (0);
		break;
		}
	    case WIDTH:
		{
		char width[ONELINE];
		/* get the next word */
		GetWord(oneLine, width);
		brush->width = atoi(width);
		break;
		}
	    case HEIGHT:	
		{
		char height[ONELINE];
		/* get the next word */
		GetWord(oneLine, height);
		brush->height = atoi(height);
		break;
		}
	    case VALUES:	
		{
		/* read in the values */
		ReadBrushValues(brushFile, &lineNum, brush);
		break;
		}
	    case FUNCS:	
		{
		/* read in the values */
		ReadBrushFunctions(brushFile, &lineNum, brush);
		break;
		}
	    default:
		{
		sprintf (errStr, "Fatal brush error... later.\n");
		fprintf(stderr, errStr);
		fclose (brushFile);
		return (0);
		}
	    }
	}
    fclose (brushFile);
    return (1);
    }

MatchKeyword( word, matchWord )
	char word[];
	char *matchWord[];

	{
	register int i = 0 ;
	while (matchWord[i][0] != NULL)
		if (strcmp( word, matchWord[i]) == 0) 
			return(i);
		else i++;
	return (-1);
	}

ReadBrushValues( bFile, lineNum, brush)
    FILE *bFile;
    int  *lineNum;
    B_PTR brush;
    {
    char	 keyWord [ONELINE];
    char    	 oneLine [ONELINE];
    register int row = 0;
    register int col = 0;
    double tmp;

    /* free it in case we've been here before */
    free((char*)&(brush->vals));
    /* Allocate all the row pointers */
    brush->vals = (int **)calloc(brush->height, sizeof(int *)); 
    for (row = 0; row < brush->height; row++) 
	{
	/* get a line */
	fgets(oneLine, 79, bFile);
	StripBlanks(oneLine);
	(*lineNum)++;
	/* skip comments ans blank lines */
	if ((*oneLine == '#') || (*oneLine == NULL)) 
	    continue;

	/* allocate a new row of brush->width cols */
	brush->vals[row] = (int *)calloc(brush->width, sizeof(int));
	for ( col = 0; col < brush->width; col++)
	    {
	    /* get first word */
	    GetWord(oneLine, keyWord);
	    /* store it away */
	    if( (*keyWord == 'C') || (*keyWord == 'c') )
		{
		brush->vals[row][col] = CUR;
		}
	    else 
		brush->vals[row][col] = (int)atoi(keyWord);

	    }
	}
    return(1);
    }

/* debugging routine */
PrintBrush( brush )
    B_PTR brush;
    {
    register int row;
    register int col;

    printf("Brush Name = %s\n", brush->name);
    printf("Brush Height = %d\n", brush->height);
    printf("Brush Width = %d\n", brush->width);

    printf("Brush Values\n");
    for (row = 0; row < brush->height; row++)
	{
	for (col = 0; col < brush->width; col++)
	    {
	    printf("%ld ", brush->vals[row][col]);
	    }
	printf("\n");
	}
    printf("Brush Functions\n");
    for (row = 0; row < brush->height; row++)
	{
	for (col = 0; col < brush->width; col++)
	    printf("%d ", brush->funcs[row][col]);
	printf("\n");
	}

    }

ReadBrushFunctions(bFile, lineNum, brush)
    FILE *bFile;
    int  *lineNum;
    B_PTR brush;
    {
    char    keyWord [ONELINE];
    char    oneLine [ONELINE];
    register int row, col;

    /* free it in case we've been here before */
    free((char *)&(brush->funcs));
    brush->funcs = (int **)calloc(brush->height, sizeof(int *));
    for (row = 0 ; row < brush->height; row++ )
	{
	fgets(oneLine, 79, bFile);
	StripBlanks( oneLine);
	(*lineNum)++;
	if ((*oneLine == '#') || (*oneLine == NULL))
		continue;

	brush->funcs[row] = (int *)calloc(brush->width, sizeof(int));
	for (col = 0; col < brush->width; col++)
	    {
	    GetWord (oneLine, keyWord);
	    StrToUpper(keyWord);
	    switch ( MatchKeyword ( keyWord, functions))
		{
		case NIL:
			brush->funcs[row][col] = NIL;
			break;
		case CPY:
			brush->funcs[row][col] = CPY;
			break;
		default:
		    printf("oops");
		}

	    }
	}
    }


/***********************************************************************

File     	:	brushio.c
Function 	:	WriteBrush(fName, brush)
Args	 	:	    char *fName; -- absolute path to brush
  	    		    B_PTR brush; -- structure to fill.

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	19 March 1990
Last Revised	:
Abstract 	:	Writes a brushfile to disk
Returns  	:	1 if successful, 0 if not..

***********************************************************************/
WriteBrush(fName, brush)
    char *fName; /* full path name to the brush */
    B_PTR brush;
    {
    FILE *bFile = (FILE *)OpenOutfile(fName);
    register int row, col;

    if(bFile == (FILE *)NULL)
	return (0);
    fprintf(bFile, "WIDTH %d\n", brush->width);
    fprintf(bFile, "HEIGHT %d\n", brush->height);
    fprintf(bFile, "VALUES\n");
    for(row = 0; row < brush->width; row++)
	{
	for(col = 0; col < brush->height; col++)
	    {
	    if (brush->vals[row][col] == (int)CUR)
		fprintf(bFile, "CUR ");
	    else
		fprintf(bFile, "%ld ", brush->vals[row][col]);
	    }
	fprintf(bFile, "\n");
	}
    fprintf(bFile, "FUNCS\n");
    for(row = 0; row < brush->width; row++)
	{
	for(col = 0; col < brush->height; col++)
	    {
	    switch (brush->funcs[row][col])
		{
		case NIL:
		    {
		    fprintf(bFile, "%s ", functions[NIL]);
		    break;
		    }
		case CPY:
		    {
		    fprintf(bFile, "%s ", functions[CPY]);
		    break;
		    }
		}
	    }
	fprintf(bFile, "\n");
	}
    fclose(bFile);
    return(1);
    }
