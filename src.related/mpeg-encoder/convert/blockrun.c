/* run a command many times on many files
 */

#include <stdio.h>

void main(int argc, char **argv)
{
    char *program;
    int numArgs;
    int firstNum;
    int lastNum;
    register int index;
    char command[1024];
    char *charPtr;
    char *charPtr2;
    char temp[50];
    int length;
    int count;
    int	skip;

    if ( argc < 6 ) {
        fprintf(stdout, "Usage:  blockrun <command> <num_args> <firstnum> <lastnum> <skip> <arg1> ... <argn>\n");
        fprintf(stdout, "Wilcard character = '='\n");
	exit(1);
    }

    program = argv[1];
    numArgs = atoi(argv[2]);
    firstNum = atoi(argv[3]);
    lastNum = atoi(argv[4]);
    skip = atoi(argv[5]);

    for ( index = firstNum; index <= lastNum; index += skip ) {
        charPtr = command;
	sprintf(charPtr, "%s ", program);
	charPtr += (strlen(program)+1);

	for ( count = 0; count < numArgs; count++ ) {
	    charPtr2 = argv[6+count];
	    while ( *charPtr2 != '\0' ) {
	        if ( *charPtr2 == '=' ) {
		    sprintf(temp, "%d", index);
		    sprintf(charPtr, "%d", index);
		    charPtr += strlen(temp);
		    charPtr2++;
		} else {
		    *charPtr = *charPtr2;
		    charPtr++;
		    charPtr2++;
		}
	    }
	    *charPtr = ' ';
	    charPtr++;
	}

	fprintf(stdout, "Executing:  %s\n", command);
	system(command);
    }
}

