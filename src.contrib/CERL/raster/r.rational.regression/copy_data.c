#include <stdio.h>

					

main()
{
	FILE *in;
	FILE *out1;
	FILE *out2;
	int i,ndata;
	double x1[90],x2[90],x3[90],y[90],t;

	in=fopen("summer89.123","r");
	out1=fopen("figure2.1","w");
	fseek(in,0L,0);
printf("test1\n"); 
	for(i=0;i<90;i++)
		{
printf("test2,i=%2d\n",i); 
		fscanf(in,"%lf%lf%lf%lf\n",&x1[i],&x2[i],&x3[i],&y[i]);
printf("test3,x1[%2d]=%g\n",i,x1[i]); 
		t=x1[i]+x2[i];
		fprintf(out1,"%10.2f\t%10.2f\n",t,x3[i]);
printf("test4,x2[%2d]=%g\n",i,x2[i]); 
		}
	fclose(in);
	fclose(out1);
}
