
/*************** ask the user a question and read response ***********/
static void ask(x,c,ptr)
char *x,c;
char *ptr;
{
char line[128];

fprintf(stderr,"%s",x);
if (c != NULL) {
	gets(line);
	switch(c) {
		case 'c':
			*ptr = line[0];
			break;
		case 'f':
			sscanf(line,"%f",ptr);
			break;
		case 's':
			sscanf(line,"%s",ptr);
			break;
		}
	}
}

p_get_cord()
{
char line[100],c,xr,yr;
float F;

sprintf(line,"Are the y coordinates relative to top, bottom or middle of the %s\n",str);

while (c != 't' && c != 'b' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter t, b, or m: ",'c',&c);
	}

switch (c) {
	case 't':
		yr = '>';
		break;
	case 'b':
		yr = NULL;
		break;
	case 'm':
		yr = '|';
		break;
	}
ask("Enter the y distance: ",'f',&F);
printf("-y %c%.3f\n",yr,F);

sprintf(line,"Are the x coordinates relative to left, right or middle of the %s\n",str);

while (c != 'l' && c != 'r' && c != 'm') {
	ask(line,NULL,NULL);
	ask("Enter l, r, or m: ",'c',&c);
	}

switch (c) {
	case 'l':
		xr = NULL;
		break;
	case 'r':
		xr = '>';
		break;
	case 'm':
		xr = '|';
		break;
	}
ask("Enter the x distance: ",'f',&F);
printf("-x %c%.3f\n",xr,F);
ask("The text can be left, right, or center justified at this coordinate.\nEnter l r or c: ",'c',&c);
printf("-j %c\n",c);
}
/**************** END t_get_cord *************************/
