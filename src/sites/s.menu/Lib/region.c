char *
format_res(x, buf, projection)
	double x;
	char *buf;
{
	G_format_resolution (x, buf, projection);
	return buf;
}
char *
format_east(x, buf, projection)
	double x;
	char *buf;
{
	G_format_easting (x, buf, projection);
	return buf;
}
char *
format_north(x, buf, projection)
	double x;
	char *buf;
{
	G_format_northing (x, buf, projection);
	return buf;
}
scan_north(buf, x)
	char *buf;
        double *x;
{
	return G_scan_northing (buf, x, G_projection());
}
scan_east(buf, x)
	char *buf;
	double *x;
{
	return G_scan_easting (buf, x, G_projection());
}

scan_res(buf, x)
        char *buf;
        double *x;
{
        return G_scan_resolution (buf, x, G_projection());
}





