scan_patcolor (buf, n, color)
    char *buf;
    int *n;
    int *color;
{
    char temp1[100], temp2[100];
    int r,g,b;

    if (sscanf (buf, "%s", temp1) != 1)
	return 0;
    if (strcmp (temp1, "color") != 0)
	return 0;

    if (sscanf(buf, "%s %d %[^\n]", temp1, n, temp2) != 3)
    {
	*n = -1;
	return -1;
    }
    if (*n < 0 || *n > 9)
    {
	*n = -1;
	return -1;
    }
    if (!scan_color (temp2, color, &r,&g,&b))
    {
	*n = -1;
	return -1;
    }
    return 1;
}
