#include<stdio.h>

debug(nrows,ncols,rm1,rr,rp1,cm1,cc,cp1,accum,chann,aspect)

int nrows, ncols;
int rm1, rr, rp1;
int cm1, cc, cp1;

int **accum;
int **chann;
int **aspect;

{
    int ch00,ch01,ch02,ch10,ch11,ch12,ch20,ch21,ch22;
    int ac00,ac01,ac02,ac10,ac11,ac12,ac20,ac21,ac22;
    int as00,as01,as02,as10,as11,as12,as20,as21,as22;

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

    as00 = 0;
    as01 = 0;
    as02 = 0;
    as10 = 0;
    as11 = aspect[rr][cc];
    as12 = 0;
    as20 = 0;
    as21 = 0;
    as22 = 0;

    if(rp1 >= nrows) {
	ch00 = -1;
	ch01 = -1;
	ch02 = -1;
	ac00 = -1;
	ac01 = -1;
	ac02 = -1;
	as00 = -1;
	as01 = -1;
	as02 = -1;
    }
    if(rm1 < 0) {
	ch20 = -1;
	ch21 = -1;
	ch22 = -1;
	ac20 = -1;
	ac21 = -1;
	ac22 = -1;
	as20 = -1;
	as21 = -1;
	as22 = -1;
    }
    if(cp1 >= ncols) {
	ch02 = -1;
	ch12 = -1;
	ch22 = -1;
	ac02 = -1;
	ac12 = -1;
	ac22 = -1;
	as02 = -1;
	as12 = -1;
	as22 = -1;
    }
    if(cm1 < 0) {
	ch00 = -1;
	ch10 = -1;
	ch20 = -1;
	ac00 = -1;
	ac10 = -1;
	ac20 = -1;
	as00 = -1;
	as10 = -1;
	as20 = -1;
    }

    ch00 = (ch00 == -1) ? ch00 : chann[rp1][cm1];
    ac00 = (ac00 == -1) ? ac00 : accum[rp1][cm1];
    as00 = (as00 == -1) ? as00 : aspect[rp1][cm1];

    ch01 = (ch01 == -1) ? ch01 : chann[rp1][cc ];
    ac01 = (ac01 == -1) ? ac01 : accum[rp1][cc ];
    as01 = (as01 == -1) ? as01 : aspect[rp1][cc ];

    ch02 = (ch02 == -1) ? ch02 : chann[rp1][cp1];
    ac02 = (ac02 == -1) ? ac02 : accum[rp1][cp1];
    as02 = (as02 == -1) ? as02 : aspect[rp1][cp1];

    ch10 = (ch10 == -1) ? ch10 : chann[rr ][cm1];
    ac10 = (ac10 == -1) ? ac10 : accum[rr ][cm1];
    as10 = (as10 == -1) ? as10 : aspect[rr ][cm1];

    ch12 = (ch12 == -1) ? ch12 : chann[rr ][cp1];
    ac12 = (ac12 == -1) ? ac12 : accum[rr ][cp1];
    as12 = (as12 == -1) ? as12 : aspect[rr ][cp1];

    ch20 = (ch20 == -1) ? ch20 : chann[rm1][cm1];
    ac20 = (ac20 == -1) ? ac20 : accum[rm1][cm1];
    as20 = (as20 == -1) ? as20 : aspect[rm1][cm1];

    ch21 = (ch21 == -1) ? ch21 : chann[rm1][cc ];
    ac21 = (ac21 == -1) ? ac21 : accum[rm1][cc ];
    as21 = (as21 == -1) ? as21 : aspect[rm1][cc ];

    ch22 = (ch22 == -1) ? ch22 : chann[rm1][cp1];
    ac22 = (ac22 == -1) ? ac22 : accum[rm1][cp1];
    as22 = (as22 == -1) ? as22 : aspect[rm1][cp1];

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
    printf("\n Azimuths:");
    printf("\n %8d %8d %8d",as00,as01,as02);
    printf("\n %8d %8d %8d",as10,as11,as12);
    printf("\n %8d %8d %8d",as20,as21,as22);
    printf("\n");
}
