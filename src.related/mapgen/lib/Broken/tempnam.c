static char *SCCSID = "@(#)tempnam.c	AMG v.1.1";
/* version V routine for BSD patch routine */
	char *
tempnam(s, ss) char *s, *ss; {
	char *t, *name, *malloc();

	name = malloc(80);
	sprintf(name,"%s/map%d.%s",s,getpid(),ss);
	return(name);
}
