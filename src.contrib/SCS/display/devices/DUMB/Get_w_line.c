/* %W%   %G% */

Get_location_with_line(cx, cy, wx, wy, button)
	int cx, cy ;
	int *wx, *wy ;
	int *button ;
{
	char c;
	char c1, c2;
	char *gets(), buf[10];

	put_chr('y');
	put_int(cx);
	put_int(cy);
	put_int(*wx);
	put_int(*wy);

	gets(buf);
	c1 = buf[0];
	c2 = buf[1];
	*wx = (c1 & 31) | ((c2 & 31) << 5);
	c1 = buf[2];
	c2 = buf[3];
	*wy = (c1 & 31) | ((c2 & 31) << 5);
	c = buf[4];
	switch (c) 
	{	case 'l': *button = 1;  break;
		case 'b': *button = 2;  break;
		case 'r': *button = 3;  break;
	}
}
