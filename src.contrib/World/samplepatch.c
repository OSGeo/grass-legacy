#define DEG (180.*.0001/3.141592654)
main(argc,argv)
char **argv;
{
	int i,mla,mlo,f,r,b;
	int p;
	int pla,plo;
	long n;
	struct { int la,lo; } ll;
	mla=atoi(argv[1]);
	mlo=atoi(argv[2]);
	while(scanf("%d%d%ld",&pla,&plo,&n)==3){
		if(mla==pla&&mlo==plo)
			goto found;
	}
	return;
found:
	f = open("world.b",0);
	b = n/512;
	r = n%512;
	seek(f,b,3);
	seek(f,r,1);
	read(f,&pla,1);
	read(f,&plo,1);
	if(pla!=mla||plo!=mlo)
		write(2,"botch\n",6);
	read(f,&p,2);
	for(i=0;i<p;i++)
	{
		read(f,&ll,4);
		printf("%.4f %.4f\n",ll.la*DEG,ll.lo*DEG);
	}
}
