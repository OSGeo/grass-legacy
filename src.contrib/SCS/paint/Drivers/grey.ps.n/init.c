#define HEADSPACE 210 /* roughly 3 * 72 per inch */
Pinit()
{
int x, y;
char buf[128];

Pnpixels(&y,&x);
Pouts("%!PS-Adobe-2.0 EPSF-2.0)\n");
Pouts( "%%%%Creator: grey.ps\n" );
Pouts( "%%%%Title: grey.ps\n");
Pouts( "%%%%Pages: 1\n" );
sprintf(buf,"%%%%BoundingBox: %d %d %d %d\n",
	0, 0, x, y+HEADSPACE);
Pouts(buf);
sprintf( buf,"%%%%EndComments\n" );
Pouts(buf);
sprintf( buf,"%%%%EndProlog\n" );
Pouts(buf);
sprintf( buf,"%%%%Page: 1 1\n" );
Pouts(buf);
Pouts("/Helvetica findfont 10 scalefont setfont\n");
}
