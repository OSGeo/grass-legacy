/***************************************************

NAME:		mount_tape ()

FUNCTION:	open tape drive for reading
****************************************************/

mount_tape (tapename)
    char *tapename;
{
    int tapefd;
    while((tapefd = open(tapename, 0)) < 0)
	I_ask("\nMount tape and put on-line. Then hit RETURN-->",0,1) ;
    return tapefd;
}
