/****************************************************************/
/*								*/
/*	input.c		in	~/src/i_range			*/
/*								*/
/*	These functions handle text input on the text line	*/
/*								*/
/*								*/

#include "menu.h"

input()

{
	int x,i,s;
	extern char buf[];
	extern int letter_width, txt_a, txt_b;
	extern struct box talk;
	char c[2];


	c[1] = '\0';
	i=0;
	x = txt_a +  strlen(buf)*letter_width;
	s = x;

	while (1)
	{
		New_tty() ;
		c[0]=getchar();
		Old_tty() ;

		if((int ) c[0] == 13)
		{
			break; 
		}

		if(c[0]=='\b'){
			if(x<=s);
			else {
				erase_letter(x,txt_b);
				x -= letter_width;
				i--;
			}

		}

		else if(x>(talk.r-3*letter_width));

		else{
			buf[i++] = c[0];
			x += letter_width;
			throw_text(c,x,txt_b);
		}
	}
	buf[i] = '\0';
	G_strip(buf);

}


erase_letter(x,y)
int x,y;
{
	int t,b,l,r,color;
	register line;
	extern int letter_height;

	t= y -5*letter_height/4;
	b= y + letter_height/3;
	l= x ;
	r= x + letter_width;

	for(line=t; line<=b; line++)
	{
		R_move_abs(l-1,line);
		R_cont_abs(r+1,line);
		R_flush();
	}

}

/****************************************************************/
