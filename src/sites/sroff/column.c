#define NCOLS 30

static int ncols = 0;
static int width[NCOLS];
static char column_separator[2] = {' ','\0'};

setcols (buf)	char *buf;
{
	char *argv[NCOLS];
	int i;

	ncols = parse (buf, argv, NCOLS, " \t");
	for (i = 0; i < ncols; i++)
		width[i] = atoi (argv[i]);
}
setcolsep(buf)	char *buf;
{
	column_separator[0] = buf[0];
}

columns (buf)	char *buf;
{
	char field[300];
	char line[1024];
	int i,n;
	int pad;
	char fill[2];

	*line = 0;
	for (i = 0; gets(buf) && strcmp(buf,".end"); i++)
	{
		if (i >= ncols)
			continue;
		if ((n = width[i]) <= 0)
			continue;
		if (n >= sizeof(field))
			n = sizeof(field) - 1;
		if (buf[0] == 0)
			buf[1] = 0;

		switch (buf[0])
		{
		case 'f':
		case 'F':
			if ((fill[0] = buf[1]) == 0)
				fill[0] = ' ';
			fill[1] = 0;
			field[0] = 0;
			for (pad = 0; pad < n; pad++)
				strcat (field, fill);
			break;
		case 'l':
		case 'L':
			sprintf (field, "%-*.*s", n, n, buf+1);
			break;
		case 'c':
		case 'C':
			if ((pad = (n - strlen(buf+1))/2) > 0)
			{
				n -= pad;
				sprintf(field, "%*s%-*.*s",
					pad, " ", n, n, buf+1);
				break;
			}
			/*FALLTHROUGH*/
		default:
			sprintf(field,"%*.*s", n, n, buf+1);
			break;
		}
		if (i > 0)
			strcat (line, column_separator);
		strcat (line, field);
	}
	strcpy (buf, line);
}
