/*======================================================================
			  draw_site_label.c
======================================================================*/
int draw_site_label(int x,
		    int y,
		    int offset,
		    int width,
		    int height,
		    int color,
		    char *font,
		    char *text,
		    char *placement)
{
  int dx,dy;
  int so;
  int top,bottom,left,right;
  int length;

  R_move_abs(x,y);
  R_text_size(width, height);
  R_standard_color(color);
  R_font(font);

  R_get_text_box(text, &top, &bottom, &left, &right);
  length = right-left;

  /* Modify the position depending on the offset and placement */
  so = (int)((double)offset/1.414);
  if(strcmp(placement, "n") == 0) {
    dx=-length/2;
    dy=-offset;
  } else if(strcmp(placement, "s") == 0) {
    dx=-length/2;
    dy=offset+height;
  } else if(strcmp(placement, "e") == 0) {
    dx=offset;
    dy=height/2;
  } else if(strcmp(placement, "w") == 0) {
    dx=-offset-length;
    dy=height/2;
  } else if(strcmp(placement, "ne") == 0) {
    dx=so;
    dy=-so;
  } else if(strcmp(placement, "nw") == 0) {
    dx=-length-so;
    dy=-so;
  } else if(strcmp(placement, "se") == 0) {
    dx=so;
    dy=height+so;
  } else if(strcmp(placement, "sw") == 0) {
    dx=-length-so;
    dy=height+so;
  } else if(strcmp(placement, "c") == 0) {
    dx=-length/2;
    dy=height/2;
  }

  R_move_rel(dx,dy);
  R_text(text);
}
