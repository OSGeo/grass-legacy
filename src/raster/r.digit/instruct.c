#define MAXLINES 18
#define LEFT "where am i"
#define MIDDLE "mark point"
#define RIGHT "done"
#define TITLE (char *) 0

static char *Title  = TITLE;
static char *Left   = LEFT;
static char *Middle = MIDDLE;
static char *Right  = RIGHT;

static int nlines = 0;

instructions(n)
{
    if (n == 0) nlines = MAXLINES;
    if (nlines >= MAXLINES)
    {
	if(Title) printf("%s\n", Title);
	printf(" Buttons:\n") ;
	printf("  Left:   %s\n", Left) ;
	printf("  Middle: %s\n", Middle) ;
	printf("  Right:  %s\n\n", Right) ;
	nlines = 0 ;
    }
    nlines += n;
}

reset_instructions()
{
    Title  = TITLE;
    Left   = LEFT;
    Middle = MIDDLE;
    Right  = RIGHT;
    nlines = 0;
}

left_button(s) char *s;
{
    Left = s;
}
middle_button(s) char *s;
{
    Middle = s;
}
right_button(s) char *s;
{
    Right = s;
}
