
#define NPLANES	10
#include <stdio.h>
#include <fcntl.h>
extern int SCREEN_LEFT	  ;
extern int SCREEN_RIGHT  ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_TOP    ;

Panel_save(name, t, b, l, r)
	char *name ;
{
	int fd;
	int planebufsize ;
	int frame ;
	int plane ;
	unsigned char *planebuf[2] ;
	int i;
	int buf ;
	int mod ;
	int row ;
	int mask;
	int size;

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


	planebufsize = (t - b) * (16 + (r - l) / 8) ;
	planebuf[0] = (unsigned char *) malloc(planebufsize * 2) ;
	if (planebuf[0] == NULL)
		return(-1) ;
	planebuf[1] = planebuf[0] + planebufsize;

	mask = umask(0);
	size = planebufsize * NPLANES + 4 * sizeof(int);
	unlink (name);
	if ((fd = open(name, O_CREAT|O_CTG, 0666, size)) < 0)
	    fd = creat (name, 0666);
	else
	{
	    close (fd);
	    fd = open (name, 1);
	}
	umask (mask);
	if (fd < 0)
	{
	    free (planebuf);
	    return -1;
	}
	mgigetfb(&frame, &mod) ;

	buf = t ; write(fd, &buf, sizeof(buf)) ;
	buf = b ; write(fd, &buf, sizeof(buf)) ;
	buf = l ; write(fd, &buf, sizeof(buf)) ;
	buf = r ; write(fd, &buf, sizeof(buf)) ;

	i = 0;
	for(plane=0; plane<NPLANES; plane++)
	{
		mgigetfbdata(frame, 1<<plane, l, b, r, t, planebuf[i]);
		if (i)
		    write(fd, *planebuf, planebufsize*2) ;
		i = !i;
	}
	if (!i)
	    write(fd, *planebuf, planebufsize) ;
	close(fd) ;
	free(planebuf) ;
	return(0) ;
}

Panel_restore(name)
	char *name ;
{
	int fd;
	int planebufsize ;
	int frame ;
	int plane ;
	unsigned char *planebuf[2] ;
	int i;
	int buf ;
	int mod ;
	int t, b, l, r ;
	int row ;

	if ((fd = open(name, 0)) < 0)
		return(-1) ;

	read(fd, &t, sizeof(t)) ;
	read(fd, &b, sizeof(b)) ;
	read(fd, &l, sizeof(l)) ;
	read(fd, &r, sizeof(r)) ;

	mgigetfb(&frame, &mod) ;

	planebufsize = (t - b) * (16 + (r - l) / 8) ;
	for (i = 0; i < 2; i++)
	{
	    planebuf[i] = (unsigned char *) malloc(planebufsize) ;
	    if (planebuf[i] == NULL)
	    {
		if (i) free (planebuf[0]);
		close(fd) ;
		return(-1) ;
	    }
	}

	i = 0;
	for(plane=0; plane<NPLANES; plane++)
	{
		read(fd, planebuf[i], planebufsize) ;
		if (plane)
		    mgisyncrb(1) ;
		mgiloadfbdata(frame, 1<<plane, l, b, r, t, planebuf[i]);
		i = !i;
	}
	mgisyncrb(1) ;

	close(fd) ;
	free(planebuf[0]) ;
	free(planebuf[1]) ;
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
