#include<stdio.h>

debug(nrows,ncols,rm1,rr,rp1,cm1,cc,cp1,accum,chann,drain,dem)

int nrows, ncols;
int rm1, rr, rp1;
int cm1, cc, cp1;

int **accum;
int **chann;
int **drain;
int **dem;

{
    int ch00,ch01,ch02,ch10,ch11,ch12,ch20,ch21,ch22;
    int ac00,ac01,ac02,ac10,ac11,ac12,ac20,ac21,ac22;
    int dr00,dr01,dr02,dr10,dr11,dr12,dr20,dr21,dr22;
    int el00,el01,el02,el10,el11,el12,el20,el21,el22;

/*
 *  Allocate space for vectors.
 */
    ch00 = 0;
    ch01 = 0;
    ch02 = 0;
    ch10 = 0;
    ch11 = chann[rr][cc];
    ch12 = 0;
    ch20 = 0;
    ch21 = 0;
    ch22 = 0;

    ac00 = 0;
    ac01 = 0;
    ac02 = 0;
    ac10 = 0;
    ac11 = accum[rr][cc];
    ac12 = 0;
    ac20 = 0;
    ac21 = 0;
    ac22 = 0;

    dr00 = 0;
    dr01 = 0;
    dr02 = 0;
    dr10 = 0;
    dr11 = drain[rr][cc];
    dr12 = 0;
    dr20 = 0;
    dr21 = 0;
    dr22 = 0;

    el00 = 0;
    el01 = 0;
    el02 = 0;
    el10 = 0;
    el11 = dem[rr][cc];
    el12 = 0;
    el20 = 0;
    el21 = 0;
    el22 = 0;

    if(rp1 >= nrows) {
	ch00 = -1;
	ch01 = -1;
	ch02 = -1;
	ac00 = -1;
	ac01 = -1;
	ac02 = -1;
	dr00 = -1;
	dr01 = -1;
	dr02 = -1;
	el00 = -1;
	el01 = -1;
	el02 = -1;
    }
    if(rm1 < 0) {
	ch20 = -1;
	ch21 = -1;
	ch22 = -1;
	ac20 = -1;
	ac21 = -1;
	ac22 = -1;
	dr20 = -1;
	dr21 = -1;
	dr22 = -1;
	el20 = -1;
	el21 = -1;
	el22 = -1;
    }
    if(cp1 >= ncols) {
	ch02 = -1;
	ch12 = -1;
	ch22 = -1;
	ac02 = -1;
	ac12 = -1;
	ac22 = -1;
	dr02 = -1;
	dr12 = -1;
	dr22 = -1;
	el02 = -1;
	el12 = -1;
	el22 = -1;
    }
    if(cm1 < 0) {
	ch00 = -1;
	ch10 = -1;
	ch20 = -1;
	ac00 = -1;
	ac10 = -1;
	ac20 = -1;
	dr00 = -1;
	dr10 = -1;
	dr20 = -1;
	el00 = -1;
	el10 = -1;
	el20 = -1;
    }

    ch00 = (ch00 == -1) ? ch00 : chann[rp1][cm1];
    ac00 = (ac00 == -1) ? ac00 : accum[rp1][cm1];
    dr00 = (dr00 == -1) ? dr00 : drain[rp1][cm1];
    el00 = (el00 == -1) ? el00 :   dem[rp1][cm1];

    ch01 = (ch01 == -1) ? ch01 : chann[rp1][cc ];
    ac01 = (ac01 == -1) ? ac01 : accum[rp1][cc ];
    dr01 = (dr01 == -1) ? dr01 : drain[rp1][cc ];
    el01 = (el01 == -1) ? el01 :   dem[rp1][cc ];

    ch02 = (ch02 == -1) ? ch02 : chann[rp1][cp1];
    ac02 = (ac02 == -1) ? ac02 : accum[rp1][cp1];
    dr02 = (dr02 == -1) ? dr02 : drain[rp1][cp1];
    el02 = (el02 == -1) ? el02 :   dem[rp1][cp1];

    ch10 = (ch10 == -1) ? ch10 : chann[rr ][cm1];
    ac10 = (ac10 == -1) ? ac10 : accum[rr ][cm1];
    dr10 = (dr10 == -1) ? dr10 : drain[rr ][cm1];
    el10 = (el10 == -1) ? el10 :   dem[rr ][cm1];

    ch12 = (ch12 == -1) ? ch12 : chann[rr ][cp1];
    ac12 = (ac12 == -1) ? ac12 : accum[rr ][cp1];
    dr12 = (dr12 == -1) ? dr12 : drain[rr ][cp1];
    el12 = (el12 == -1) ? el12 :   dem[rr ][cp1];

    ch20 = (ch20 == -1) ? ch20 : chann[rm1][cm1];
    ac20 = (ac20 == -1) ? ac20 : accum[rm1][cm1];
    dr20 = (dr20 == -1) ? dr20 : drain[rm1][cm1];
    el20 = (el20 == -1) ? el20 :   dem[rm1][cm1];

    ch21 = (ch21 == -1) ? ch21 : chann[rm1][cc ];
    ac21 = (ac21 == -1) ? ac21 : accum[rm1][cc ];
    dr21 = (dr21 == -1) ? dr21 : drain[rm1][cc ];
    el21 = (el21 == -1) ? el21 :   dem[rm1][cc ];

    ch22 = (ch22 == -1) ? ch22 : chann[rm1][cp1];
    ac22 = (ac22 == -1) ? ac22 : accum[rm1][cp1];
    dr22 = (dr22 == -1) ? dr22 : drain[rm1][cp1];
    el22 = (el22 == -1) ? el22 :   dem[rm1][cp1];

    printf("\n Stream Indicies:");
    printf("\n %8d %8d %8d",ch00,ch01,ch02);
    printf("\n %8d %8d %8d",ch10,ch11,ch12);
    printf("\n %8d %8d %8d",ch20,ch21,ch22);
    printf("\n");
    printf("\n Accumul Indicies:");
    printf("\n %8d %8d %8d",ac00,ac01,ac02);
    printf("\n %8d %8d %8d",ac10,ac11,ac12);
    printf("\n %8d %8d %8d",ac20,ac21,ac22);
    printf("\n");
    printf("\n Drainage:");
    printf("\n %8d %8d %8d",dr00,dr01,dr02);
    printf("\n %8d %8d %8d",dr10,dr11,dr12);
    printf("\n %8d %8d %8d",dr20,dr21,dr22);
    printf("\n");
    printf("\n Elevations:");
    printf("\n %8d %8d %8d",el00,el01,el02);
    printf("\n %8d %8d %8d",el10,el11,el12);
    printf("\n %8d %8d %8d",el20,el21,el22);
    printf("\n");
}
