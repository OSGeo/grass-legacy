extern double sin();
extern double cos();
double ang;
int mrad, subrad, divs, i;
int j[3];
main(argc,argv)  int argc; char **argv;
{

extern int atoi();

mrad=atoi(*++argv);
subrad=atoi(*++argv);
divs=atoi(*++argv);

j[2]= subrad;

for (ang=0.0;  ang<6.283; ang=+6.283/divs)
{	j[0]=sin(ang)*mrad;
	j[1]=cos(ang)*mrad;
	write(1,"c", 1);
	write(1, j, 6);
}
}
