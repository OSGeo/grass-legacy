#include "P.h"
char buf[255];

Pinit()
{
	int i,j,k,count = 0; 
	int maxcolors, division;	


	ncolumns = 300;
	maxcolumns = ncolumns * 8;

	/* Basic Initialization */
	esc("%-12345X"); 
	sprintf(buf,"@PJL ENTER LANGUAGE = PCL%c",10);
	Pouts(buf);     /* Select PCL Language */
	esc("%0A");     /* Place in PCL mode */
	esc("&l1H");    /* Source of Paper: Tray */
	sprintf(buffer,"*t%dR",ncolumns);
	esc(buffer);  /* Set to 300 DPI */
	esc("*t7J");    /* Select Render Algorithm */
	esc("*v10");
	esc("*v1N");
	esc("*l1R");
	esc("&l1x0M");
	esc("*t0I");
	esc("&l0o2a4d1e42F");
	esc("*r0F");    /* Use Logical Orientation of Page */
	esc("*p0X");    /* Move X to 0 Position */
	esc("*p0Y");    /* Move Y to 0 Position */
	esc("*c0T");    /* Set anchor point to current PCL cursor */

	/*  Set the HP-GL2 Window */
	esc("%0B");
	Pouts("IP0,0,1016,1016;SC0,300,0,300;");

	/* Page Setup Information */
	esc("%0A");
	esc("*o0Q");	

	/* Set the Color Index Table */
	esc("*p0P");     /* Push Palette */
	sprintf(buf,"%c*v6W%c%c%c%c%c%c",0x1b,0x00,0x01,0x08,0x08,0x08,0x08);
	Pout(buf,11);

	/* Set the Color Table */
	Pset_color_levels(NCOLORS);
	maxcolors = (NCOLORS*NCOLORS*NCOLORS)-1;
	division = 255 / NCOLORS;
	for (i = 0; i< NCOLORS; i++)  {
		for (j = 0; j < NCOLORS; j++)  {
			for (k = 0; k < NCOLORS; k++)  {
				if (count == maxcolors) continue;
				sprintf(buf,"*v%da%db%dc%dI",i*division,
					j*division,k*division,count++);
				esc(buf);
			}
		}
	}
	sprintf(buf,"*v255a255b255c%dI",maxcolors);
	esc(buf);
}
