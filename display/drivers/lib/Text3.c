/* text draw truetypefont
*
* 2004/01/30
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "config.h"
#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif

#ifdef HAVE_FT2BUILD_H
#include <ft2build.h>
#include FT_FREETYPE_H
#endif

#include "driver.h"
#include "driverlib.h"

/*#define DEBUG_LOG(S) {FILE *fp = fopen("debug.TXT","a");fputs(S,fp);fclose(fp);}*/
/*#define DEBUG_LOG_INT(D) {FILE *fp = fopen("debug.TXT","a");fprintf(fp,"%d",D);fclose(fp);}*/
/*#define DEBUG_LOG_DOUBLE(D) {FILE *fp = fopen("debug.TXT","a");fprintf(fp,"%f",D);fclose(fp);}*/

#ifdef HAVE_FT2BUILD_H
static int convert_str(char*,char*,unsigned char**);
static void release_convert_str(unsigned char*);
static int setMatrix(FT_Matrix*,double);
static int draw_text(FT_Face,FT_Vector*,FT_Matrix*,unsigned char*,int,int);
static void draw_bitmap(FT_Bitmap*,FT_Int,FT_Int);
static void set_text_box(FT_Bitmap*,FT_Int,FT_Int);
#endif

static int fdont_draw = 0;
static int ft, fb, fl, fr;

static int drawMain(int x,int y,double text_size_x,double text_size_y,double text_rotation,char *string)
{
#ifdef HAVE_FT2BUILD_H
	FT_Library		library;
	FT_Face			face;
	FT_Matrix		matrix;
	/*FT_UInt		glyph_index;*/
	FT_Vector		pen;
	FT_Error		ans;
	char*           filename;
	char*           charset;

	unsigned char*	out;
	int outlen;

	/* get file name */
	filename = getFreeTypeName();
	charset = getCharset();

	/* set freetype */
	ans = FT_Init_FreeType(&library);
	if(ans) {
		/* DEBUG_LOG("Text3 error: ft init\n"); */
		return -1;
	}
	ans = FT_New_Face(library,filename,0,&face);
	if(ans==FT_Err_Unknown_File_Format) {
		/* DEBUG_LOG("Text3 error: ft new face 1\n"); */
		FT_Done_FreeType(library);
		return -1;
	}else if(ans) {
		/* DEBUG_LOG("Text3 error: ft new face 2\n"); */
		FT_Done_FreeType(library);
		return -1;
	}

	/* ans = FT_Set_Pixel_Sizes(face,10,10); */
	/* ans = FT_Set_Char_Size(face,text_size_x*64,text_size_y*64,0,0); */
	/* ans = FT_Set_Char_Size(face,10*64,0,72,0); */
	/* ans = FT_Set_Char_Size(face,text_size_x*64,text_size_y*64,72,72); */
	ans = FT_Set_Char_Size(face,text_size_x*64,text_size_y*64,100,100);
	/*
	ans = FT_Set_Pixel_Sizes(
            face,
			0,
            (int)text_size_y );
	*/
	if(ans) {
		/* DEBUG_LOG("Text3 error: ft set size\n"); */
		FT_Done_Face(face);
		FT_Done_FreeType(library);
		return -1;
	}

	/* init point */
	pen.x = x*64;
	/* pen.y = 0; */
	pen.y = (screen_bottom-y)*64;

	/* convert string to:shift-jis from:charset */
	outlen = convert_str(charset,string,&out);

	/* set matrix */
	setMatrix(&matrix,text_rotation);
	/* draw */
	draw_text(face,&pen,&matrix,out,outlen,0);

	/* release */
	release_convert_str(out);

	/* FT_done */
	FT_Done_Face(face);
	FT_Done_FreeType(library);
#endif

	return 0;
}

#ifdef HAVE_FT2BUILD_H
static int setMatrix(FT_Matrix* matrix,double rotation)
{
	matrix->xx = (FT_Fixed)( cos(rotation)*0x10000);
	matrix->xy = (FT_Fixed)(-sin(rotation)*0x10000);
	matrix->yx = (FT_Fixed)( sin(rotation)*0x10000);
	matrix->yy = (FT_Fixed)( cos(rotation)*0x10000);

	return 0;
}

static int convert_str(char* from,char* in,unsigned char** out)
{
	iconv_t cd;
	size_t ret;
	int len = 0;
	int i = 0;
	int res = 0;
	unsigned char* p1;
	unsigned char* p2;

	len = strlen(in);
	res = 2*(len+1);
/* 	res = 4*(len+1); */

	*out = (unsigned char*)malloc(res);
	memset(*out,res,0);
	p1 = in;
	p2 = *out;

	i = res;
	if( (cd = iconv_open("UCS-2BE",from)) < 0 ) {
/* 	if( (cd = iconv_open("UCS-4",from)) < 0 ) { */
/* 	if( (cd = iconv_open("UTF-8",from)) < 0 ) { */
		return -1;
	}
	ret = iconv(cd,(char**)&p1,&len,(char**)&p2,&i);
	/*
	if(ret == -1){
		if(errno == E2BIG){
			{DEBUG_LOG("E2BIG\n")}
		}
		if(errno == EILSEQ){
			{DEBUG_LOG("EILSEQ\n")}
		}
		if(errno == EINVAL){
			{DEBUG_LOG("EILSEQ\n")}
		}
	}
	*/
	iconv_close(cd);

	res -= i;

	return res;
}

static void release_convert_str(unsigned char* out)
{
	free(out);
}

static int draw_text(FT_Face face,FT_Vector* pen,FT_Matrix* matrix,unsigned char* out,int len,int color)
{
	int i=0;
	FT_ULong ch;
	FT_Error ans;
	FT_GlyphSlot  slot = face->glyph;

	for(i=0;i<len;i+=2) {
		ch = (out[i]<<8)|out[i+1];
		if(ch == 10) continue;
		/* transform */
		FT_Set_Transform(face,matrix,pen);
		/* get glyph image */
		ans = FT_Load_Char(face,ch,FT_LOAD_NO_BITMAP);
		if(ans) continue;
   		ans = FT_Render_Glyph( face->glyph, ft_render_mode_normal );
   		if ( ans ) continue;
		/* draw bitmap */
		if(fdont_draw==0) {
			draw_bitmap(&slot->bitmap,slot->bitmap_left,screen_bottom-slot->bitmap_top);
		}else{
			set_text_box(&slot->bitmap,slot->bitmap_left,screen_bottom-slot->bitmap_top);
		}

		/* increment pen position */
		pen->x += slot->advance.x;
		pen->y += slot->advance.y;
	}
	return 0;
}

static void set_text_box(FT_Bitmap* bitmap,FT_Int x,FT_Int y)
{
	FT_Int xMax = x + bitmap->width;
	FT_Int yMax = y + bitmap->rows;
	if( (x == xMax) || (y == yMax) ) return;
	if(x<fl) fl = x;
	if(xMax>fr) fr = xMax;
	if(y<ft) ft = y;
	if(yMax>fb) fb = yMax;
	return;
}

static void draw_bitmap(FT_Bitmap* bitmap,FT_Int x,FT_Int y)
{
	FT_Int i,j,p,q;
	unsigned char color;
	FT_Int xMax = x + bitmap->width;
	FT_Int yMax = y + bitmap->rows;
	for(i=x,p=0;i<xMax;i++,p++) {
		for(j=y,q=0;j<yMax;j++,q++) {
			color = bitmap->buffer[q*bitmap->width+p];
			if(color>128) {
				draw_point(i,j);
			}else{
			}
		}
	}
	return;
}
#endif

int soft_text_freetype(int x,int y,double text_size_x,double text_size_y,double text_rotation,char *string)
{
	text_size_x *= 25.0;
	text_size_y *= 25.0;
	drawMain(x,y,text_size_x,text_size_y,text_rotation,string);
	return 0;
}

int soft_text_ext_freetype(int x,int y,double text_size_x,double text_size_y,double text_rotation,char *string)
{
	fdont_draw = 1;
	text_size_x *= 25.0;
	text_size_y *= 25.0;
	ft = 999999;
	fb = 0;
	fl = 999999;
	fr = 0;
	/* drawMain(x,y,text_size_x,text_size_y,text_rotation,string); */
	/* drawMain(0,0,text_size_x,text_size_y,text_rotation,string); */
	drawMain(0,y,text_size_x,text_size_y,text_rotation,string);
	/* ft += y; */
	/* fb += y; */
	fl += x;
	fr += x;
	fdont_draw = 0;
	return 0;
}

int get_text_ext_freetype(int *top,int *bot,int *left,int *rite)
{
	*top = ft;
	*bot = fb;
	*left = fl;
	*rite = fr;
	return 0;
}

