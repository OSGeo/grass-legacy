

/*********************/
/* TODO:  port this SG3D code to nviz */
/*********************/

#include <stdio.h>
#include <fmclient.h>

static fmfonthandle Curfont;
static fmfontinfo Curinfo;
static int font_library_init=0;

/* #define SET_FONT */

/*
  Set the font for labels and scales:
     font - 1 = times
            2 = helvetica
	    3 = courier
     bold - true = make font bold
     italic - true = make font italic
     siz - font_size (will be scaled)
 */
void gs_set_font(int font, int bold, int italic, double siz) {
  char *s;

  if (!font_library_init) {
    fminit();
    font_library_init=1;
  }
  
  if (Curfont){
    fmfreefont(Curfont);
    Curfont = NULL;
  }

  switch (font) {
  case 1: /* times */
    if (bold) {
      if (italic) {
	if(0 == (Curfont = fmfindfont("Times-BoldItalic")))
	  fprintf(stderr,"Times-BoldItalic font unavailable\n");
      } else {
	if(0 == (Curfont = fmfindfont("Times-Bold")))
	  fprintf(stderr,"Times-Bold font unavailable\n");
      }   
    } else if (italic) {
      if(0 == (Curfont = fmfindfont("Times-Italic")))
	fprintf(stderr,"Times-Italic font unavailable\n");
    } else
      if(0 == (Curfont = fmfindfont("Times-Roman")))
	fprintf(stderr,"Times-Roman font unavailable\n");
    break;

  case 2: /* helvetica */
    if (bold) {
      if (italic) {
	if(0 == (Curfont = fmfindfont("Helvetica-BoldOblique")))
	  fprintf(stderr,"Helvetica-BoldOblique font unavailable\n");
      } else {
	if(0 == (Curfont = fmfindfont("Helvetica-Bold")))
	  fprintf(stderr,"Helvetica-Bold font unavailable\n");
      }
    } else if (italic) {
      if(0 == (Curfont = fmfindfont("Helvetica-Oblique")))
	fprintf(stderr,"Helvetica-Oblique font unavailable\n");
    } else
      if(0 == (Curfont = fmfindfont("Helvetica")))
	fprintf(stderr,"Helvetica font unavailable\n");
    break;

  case 3: /* courier */
    if (bold) {
      if (italic) {
	if(0 == (Curfont = fmfindfont("Courier-BoldOblique")))
	  fprintf(stderr,"Courier-BoldOblique font unavailable\n");
      } else {
	if(0 == (Curfont = fmfindfont("Courier-Bold")))
	  fprintf(stderr,"Courier-Bold font unavailable\n");
      }
    } else if (italic) {
      if(0 == (Curfont = fmfindfont("Courier-Oblique")))
	fprintf(stderr,"Courier-Oblique font unavailable\n");
    } else
      if(0 == (Curfont = fmfindfont("Courier")))
	fprintf(stderr,"Courier font unavailable\n");
    break;

  }
  
  if(Curfont){    
    Curfont = fmscalefont(Curfont, siz);
    fmsetfont(Curfont);
    fmgetfontinfo(Curfont, &Curinfo);
  }
}

font_is_set() {
  if(Curfont)
    return(1);
  return(0);
}

get_txtwidth(char *s) {

  if(!Curfont) return(strwidth(s));
  
  return(fmgetstrwidth(Curfont, s));

}

get_txtheight() {
  
  if(!Curfont) return(16); /* default font */
  
  return(Curinfo.ysize);
}

get_txtdescender() {

  if(!Curfont) return(4);

  return(Curinfo.yorig);
}

get_txtxoffset() {

  if(!Curfont) return(0);
  
  return(Curinfo.xorig);
}










