#define NPLANES	10
#include <stdio.h>
extern int SCREEN_LEFT	  ;
extern int SCREEN_RIGHT  ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_TOP    ;

Panel_save(name, t, b, l, r)
	char *name ;
{
	FILE *fopen() ;
	FILE *fd ;
	int planebufsize ;
	int frame ;
	int plane ;
	unsigned char *planebuf ;
	int buf ;
	int mod ;
	int row ;

/* Make sure request is within screen */
	if (l < SCREEN_LEFT  ) l = SCREEN_LEFT ;
	if (r > SCREEN_RIGHT ) r = SCREEN_RIGHT ;
	if (t < SCREEN_TOP   ) t = SCREEN_TOP ;
	if (b > SCREEN_BOTTOM) b = SCREEN_BOTTOM ;

/* Flip coordinate system;  GRASS: 0 top   MASS: 0 bottom */
	t = SCREEN_BOTTOM - t ;
	b = SCREEN_BOTTOM - b ;

/* Round off x to nearest 16 bit boundary - necessary */
	l = l - ( l % 16 ) ;
	r = r + 16 - ( r % 16 ) ;
	if (l < SCREEN_LEFT  ) l += 16 ;
	if (r > SCREEN_RIGHT ) r -= 16 ;

	if (NULL == (fd = fopen(name, "w")))
		return(-1) ;

	planebufsize = (t - b) * (16 + (r - l) / 8) ;
	planebuf = (unsigned char *) malloc(planebufsize) ;
	if (planebuf == NULL)
	{
		fclose(fd) ;
		return(-1) ;
	}

	mgigetfb(&frame, &mod) ;

	buf = t ; fwrite(&buf, sizeof(buf), 1, fd) ;
	buf = b ; fwrite(&buf, sizeof(buf), 1, fd) ;
	buf = l ; fwrite(&buf, sizeof(buf), 1, fd) ;
	buf = r ; fwrite(&buf, sizeof(buf), 1, fd) ;

	for(plane=0; plane<NPLANES; plane++)
	{
		mgigetfbdata(frame, 1<<plane, l, b, r, t, planebuf);
		fwrite(planebuf, sizeof(*planebuf), planebufsize, fd) ;
	}
	fclose(fd) ;
	free(planebuf) ;
	return(0) ;
}

Panel_restore(name)
	char *name ;
{
	FILE *fopen() ;
	FILE *fd ;
	int planebufsize ;
	int frame ;
	int plane ;
	unsigned char *planebuf ;
	int buf ;
	int mod ;
	int t, b, l, r ;
	int row ;

	if (NULL == (fd = fopen(name, "r")))
		return(-1) ;

	fread(&t, sizeof(t), 1, fd) ;
	fread(&b, sizeof(b), 1, fd) ;
	fread(&l, sizeof(l), 1, fd) ;
	fread(&r, sizeof(r), 1, fd) ;

	mgigetfb(&frame, &mod) ;

	planebufsize = (t - b) * (16 + (r - l) / 8) ;
	planebuf = (unsigned char *) malloc(planebufsize) ;
	if (planebuf == NULL)
	{
		fclose(fd) ;
		return(-1) ;
	}

	for(plane=0; plane<NPLANES; plane++)
	{
		fread(planebuf, sizeof(*planebuf), planebufsize, fd) ;
		mgiloadfbdata(frame, 1<<plane, l, b, r, t, planebuf);
		mgisyncrb(1) ;
	}

	fclose(fd) ;
	free(planebuf) ;
	return(0) ;
}

Panel_delete(name)
	char *name ;
{
	unlink(name) ;
}

Panel_delete_all()
{
}
