#define YBIAS	-6
main()
{
	int ay ;
	double _text_size_y ;
	register short int *base ;
	short int b ;
	double t ;

	b = 5 ;
	base = &b ;
	_text_size_y = 3.75 ;

	ay = (int)(_text_size_y * (double)(( *base       & 0x3F) + YBIAS)) / 2 ;
printf("ay:%d tsy: %5.2lf bas:%d ybias:%d\n", ay, _text_size_y, *base & 0x3F, YBIAS) ;
}
