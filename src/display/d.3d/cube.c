main()
{
	double  rotate[3] [3];
	float   x[8], y[8], z[8];
	int     ix[8], iy[8], iz[8];
	float   rx, ry, rz;
	float	red[6], blu[6], grn[6] ;
	int disp, mod ;

	double  sin(), cos();

	int segarray[1000];
	int s1x[5], s2x[5], s3x[5], s4x[5], s5x[5];
	int        s1y[5], s2y[5], s3y[5], s4y[5], s5y[5];
	
	char  color[40];
	int   side[5];

	int   i, xl, yb, xr, yt, xc, yc, size, used;

	mgiasngp(0,0);

	printf("size of the cube  ");
	scanf("%d",&size);
	printf("0");

/*
	mgifreesets();
	for (i = 1; i <= 5; i++) {
				printf("color for side # %d  ",i);
				scanf("%s", color);
				side[i] = mgidynset(mgfcns(color));
	}
	mgifixsets();
*/

/* Set colors */
	red[0] = 1.0 ; blu[0] = 0.0 ; grn[0] = 0.0 ;
	red[1] = 0.0 ; blu[1] = 1.0 ; grn[1] = 0.0 ;
	red[2] = 0.0 ; blu[2] = 0.0 ; grn[2] = 1.0 ;
	red[3] = 1.0 ; blu[3] = 1.0 ; grn[3] = 0.0 ;
	red[4] = 0.0 ; blu[4] = 1.0 ; grn[4] = 1.0 ;
	red[5] = 1.0 ; blu[5] = 0.0 ; grn[5] = 1.0 ;
	Reset_colors(red, grn, blu, 6) ;

	mgidyndisp(1);
	mgiids();

	i    = size/2;

	x[1] = -i;
	x[2] = -i;
	x[3] =  i;
	x[4] =  i;
	x[5] = -i;
	x[6] = -i;
	x[7] =  i;
	x[8] =  i;

	y[1] =  i;
	y[2] = -i;
	y[3] = -i;
	y[4] =  i;
	y[5] =  i;
	y[6] = -i;
	y[7] = -i;
	y[8] =  i;

	z[1] = -i;
	z[2] = -i;
	z[3] = -i;
	z[4] = -i;
	z[5] =  i;
	z[6] =  i;
	z[7] =  i;
	z[8] =  i;

	mgigetvcoor(2,&xl,&yb,&xr,&yt,&used);
	xc = (xr - xl)/2;
	yc = (yt - yb)/2;

	printf("please enter the x, y, z rotation angles  ");
	scanf("%f %f %f", &rx, &ry, &rz);
	printf("0");

	rotate[1] [1] = cos(ry) * cos(rz);
	rotate[1] [2] = cos(rx) * -sin(rz) + sin(rx) * sin(ry) * cos(rz);
	rotate[1] [3] = -sin(rx) * -sin(rx) + cos(rx) * sin(ry) * cos(rz);
	rotate[2] [1] = cos(ry) * sin(rz);
	rotate[2] [2] = cos(rx) * cos(rz) + sin(rx) * sin(ry) * sin(rz);
	rotate[2] [3] = -sin(rx) * cos(rz) + cos(rz) * sin(ry) * sin(rz);
	rotate[3] [1] = -sin(ry);
	rotate[3] [2] = sin(rx) * cos(ry);
	rotate[3] [3] = cos(rx) * cos(ry);

	mgibseg(segarray,1000);

/*
	mgiset(side[1]);
*/
	mgihue(1+1);
	mgals(mgflit(5),mgfadd(s1x),mgfadd(s1y));

/*
	mgiset(side[2]);
*/
	mgihue(2+1);
	mgals(mgflit(5),mgfadd(s2x),mgfadd(s2y));

/*
	mgiset(side[3]);
*/
	mgihue(3+1);
	mgals(mgflit(5),mgfadd(s3x),mgfadd(s3y));

/*
	mgiset(side[4]);
*/
	mgihue(4+1);
	mgals(mgflit(5),mgfadd(s4x),mgfadd(s4y));
	
/*
	mgiset(side[5]);
*/
	mgihue(5+1);
	mgals(mgflit(4),mgfadd(s5x),mgfadd(s5y));


	mgidci();
	mgidseg();

	mgifb(1,2) ;

for  (;;) {

	for ( i = 1;i <= 8; i++) {
		rx = x[i];
		ry = y[i];
		rz = z[i];
		x[i] = rx * rotate[1] [1] + ry * rotate[1] [2] + rz * rotate[1] [3];
		y[i] = rx * rotate[2] [1] + ry * rotate[2] [2] + rz * rotate[2] [3];
		z[i] = rx * rotate[3] [1] + ry * rotate[3] [2] + rz * rotate[3] [3];
		ix[i] = x[i] + xc;
		iy[i] = y[i] + yc;
	}
				
	copy(s1x, ix, 1, 2, 3, 4);
	copy(s1y, iy, 1, 2, 3, 4);
	copy(s2x, ix, 1, 5, 8, 4);
	copy(s2y, iy, 1, 5, 8, 4);
	copy(s3x, ix, 1, 2, 6, 5);
	copy(s3y, iy, 1, 2, 6, 5);
	copy(s4x, ix, 5, 6, 7, 8);
	copy(s4y, iy, 5, 6, 7, 8);
	copy(s5x, ix, 2, 6, 7, 3);
	copy(s5y, iy, 2, 6, 7, 3);

	mgiwseg(segarray);
	/*
	sleep(1);
	*/

	mgiseg(segarray);
	mgigetfb(&disp, &mod) ;
	mgifb(mod, disp) ;
	mgiclearpln(2,-1,0);

	}
}

copy(sarray, darray, i1, i2, i3, i4)
int      sarray[5], darray[8], i1, i2, i3, i4;
{
		sarray[0] = darray[i1];
		sarray[1] = darray[i2];
		sarray[2] = darray[i3];
		sarray[3] = darray[i4];
		sarray[4] = darray[i1];
}

int mgi_INDEX[1024] ;

Reset_colors(red, grn, blu, tot_colr)
	float *red, *grn, *blu ;
	int tot_colr ;
{
	int incr ;

	for (incr=0; incr<=tot_colr; incr++)
	mgi_INDEX[incr+1] = 
		(_norm(red[incr]) << 20) +
		(_norm(grn[incr]) << 12) +
		(_norm(blu[incr]) <<  4) ;
	mgicms(0, tot_colr+2, mgi_INDEX) ;
}

Reset_color(red, grn, blu, number)
	float red, grn, blu ;
	int number ;
{
		mgicm(number+1,
			(_norm(red) << 20) +
			(_norm(grn) << 12) +
			(_norm(blu) <<  4) ) ;
}

_norm(intensity)
	float intensity ;
{
	return ( (int)(intensity * 15 +.5) ) ;
}
