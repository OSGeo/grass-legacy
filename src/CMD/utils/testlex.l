%{
int	num_lines=0,
	num_chars=0;
%}

%%

\n	++num_lines; ++num_chars;
.	++num_chars;

%%

main()
{
	yylex();
	(void)printf("lines #:%d, chars #:%d\n",num_lines,num_chars);
	exit(0);
}
