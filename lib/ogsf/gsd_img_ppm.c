/* - added little/big endian test Markus Neteler
 * -modified to PPM by Bob Covill <bcovill@tekmap.ns.ca>
 *
 * $Id$ 
 *
 * changed 10/99 Jaro
 * Created new function GS_write_ppm based
 * on RGB dump 
 */

#include <stdlib.h>
#include <stdio.h>

#include "local_proto.h"
#include "image.h"
#include <grass/gstypes.h>

/* FFMPEG stuff */
#ifdef HAVE_FFMPEG
#include <avcodec.h>

#define DEFAULT_BUFFER_SIZE 1000*1000

AVCodec *codec;
AVCodecContext *c= NULL;
int lav_size, out_size, frame_no;
FILE *fmpg;
AVFrame *picture;
unsigned char *outbuf, *picture_buf;
#endif
/* end FFMPEG stuff */

static unsigned short rbuf[8192];
static unsigned short gbuf[8192];
static unsigned short bbuf[8192];

int GS_write_ppm(char *name)
{
    int y, x;
    unsigned int xsize, ysize;
    FILE *fp;
    unsigned long *pixbuf;
    int swapFlag;

    /* endian test */
    swapFlag = G_is_little_endian();

    gsd_getimage(&pixbuf, &xsize, &ysize);


    if (NULL == (fp = fopen(name, "w"))) {
	fprintf(stderr, "Cannot open file for output\n"), exit(1);
    }

    fprintf(fp, "P6 %d %d 255\n", xsize, ysize);

    for (y = ysize - 1; y >= 0; y--) {
	for (x = 0; x < xsize; x++) {
	    if (!swapFlag) {
		/* big endian: SUN et al. */
		rbuf[x] = (pixbuf[y * xsize + x] & 0xFF000000) >> 24;
		gbuf[x] = (pixbuf[y * xsize + x] & 0x00FF0000) >> 16;
		bbuf[x] = (pixbuf[y * xsize + x] & 0x0000FF00) >> 8;
	    }
	    else {
		/* little endian: Linux et al. */
		rbuf[x] = (pixbuf[y * xsize + x] & 0x000000FF);
		gbuf[x] = (pixbuf[y * xsize + x] & 0x0000FF00) >> 8;
		bbuf[x] = (pixbuf[y * xsize + x] & 0x00FF0000) >> 16;
	    }
	    fputc((int) rbuf[x], fp);
	    fputc((int) gbuf[x], fp);
	    fputc((int) bbuf[x], fp);
	}

    }
    free(pixbuf);
    fclose(fp);

    return (0);
}

int GS_write_zoom(char *name, unsigned int xsize, unsigned int ysize)
{
    int y, x;
    FILE *fp;
    unsigned long *pixbuf;
    int swapFlag;


    /* endian test */
    swapFlag = G_is_little_endian();

    gsd_writeView(&pixbuf, xsize, ysize);

    if (NULL == (fp = fopen(name, "w"))) {
	fprintf(stderr, "Cannot open file for output\n"), exit(1);
    }

    fprintf(fp, "P6 %d %d 255\n", xsize, ysize);

    for (y = ysize - 1; y >= 0; y--) {
	for (x = 0; x < xsize; x++) {
	    if (!swapFlag) {
		/* big endian: SUN et al. */
		rbuf[x] = (pixbuf[y * xsize + x] & 0xFF000000) >> 24;
		gbuf[x] = (pixbuf[y * xsize + x] & 0x00FF0000) >> 16;
		bbuf[x] = (pixbuf[y * xsize + x] & 0x0000FF00) >> 8;
	    }
	    else {
		/* little endian: Linux et al. */
		rbuf[x] = (pixbuf[y * xsize + x] & 0x000000FF);
		gbuf[x] = (pixbuf[y * xsize + x] & 0x0000FF00) >> 8;
		bbuf[x] = (pixbuf[y * xsize + x] & 0x00FF0000) >> 16;
	    }

	    fputc((int) rbuf[x], fp);
	    fputc((int) gbuf[x], fp);
	    fputc((int) bbuf[x], fp);
	}

    }
    free(pixbuf);
    fclose(fp);

    return (0);
}


/******************************************
 * initialize FAME setup mpeg defaults and
 * open file for writing
******************************************/
int gsd_init_mpeg(char *name)
{
#ifdef HAVE_FFMPEG
GLuint l, r, b, t;
GLint tmp[4];

        glGetIntegerv(GL_VIEWPORT, tmp);
        l = tmp[0];
        r = tmp[0] + tmp[2] - 1;
        b = tmp[1];
        t = tmp[1] + tmp[3] - 1;

	fprintf(stderr, "Opening MPEG stream\n");

        avcodec_init();

        register_avcodec(&mpeg1video_encoder); /* just mpeg1 */
	/* -- use with othe formats ...
	avcodec_register_all();
	*/

        /* find the mpeg1 video encoder */
        codec = avcodec_find_encoder(CODEC_ID_MPEG1VIDEO);
        if (!codec) {
                fprintf(stderr, "codec not found\n");
                return(-1);
        }

        c= avcodec_alloc_context();
        picture= avcodec_alloc_frame();

        /* put sample parameters */
        c->bit_rate = 400000;
        /* resolution must be a multiple of two */
        c->width = r - l + 1;
        c->height = t - b + 1;
        /* frames per second */
	/* old 
        c->frame_rate = 25;
        c->frame_rate_base= 1;
	*/
	c->time_base= (AVRational){1,25};
        c->gop_size = 10; /* emit one intra frame every ten frames */
        c->max_b_frames=1;
        c->pix_fmt = PIX_FMT_YUV420P;

        /* open it */
        if (avcodec_open(c, codec) < 0) {
                fprintf(stderr, "could not open codec\n");
                return(-1);
        }

        if (NULL == (fmpg = fopen(name, "wb"))) {
                fprintf(stderr, "Cannot open file for output\n");
                return(-1);
        }

        outbuf = (unsigned char *) malloc(DEFAULT_BUFFER_SIZE);
        lav_size = c->width * c->height;
        picture_buf = malloc((lav_size * 3) / 2); /* size for YUV 420 */
        picture->data[0] = picture_buf;
        picture->data[1] = picture->data[0] + lav_size;
        picture->data[2] = picture->data[1] + lav_size / 4;
        picture->linesize[0] = c->width;
        picture->linesize[1] = c->width / 2;
        picture->linesize[2] = c->width / 2;
#else
	fprintf(stderr, "NVIZ has not been built with MPEG output support\n");
	return(-1);
#endif

        return (0);

}


/*********************************************
 * get RGB pixbuf and convert to YUV 4:2:0
 * image and write to mpeg stream
*********************************************/
int gsd_write_mpegframe()
{
#ifdef HAVE_FFMPEG
unsigned int xsize, ysize;
int x, y, xy, xy_uv;
int length;
int yy, uu, vv;
unsigned long *pixbuf;
int swapFlag;

        /* endian test */
        swapFlag = G_is_little_endian();

        gsd_getimage(&pixbuf, &xsize, &ysize);
        xy = xy_uv = 0;
        for (y = ysize - 1; y >= 0; y--) {
          for (x = 0; x < xsize; x++) {
            if (!swapFlag) {
                /* big endian: SUN et al. */
                rbuf[x] = (pixbuf[y * xsize + x] & 0xFF000000) >> 24;
                gbuf[x] = (pixbuf[y * xsize + x] & 0x00FF0000) >> 16;
                bbuf[x] = (pixbuf[y * xsize + x] & 0x0000FF00) >> 8;
            }
            else {
                /* little endian: Linux et al. */
                rbuf[x] = (pixbuf[y * xsize + x] & 0x000000FF);
                gbuf[x] = (pixbuf[y * xsize + x] & 0x0000FF00) >> 8;
                bbuf[x] = (pixbuf[y * xsize + x] & 0x00FF0000) >> 16;
            }
            yy = (0.257 * rbuf[x]) + (0.504 * gbuf[x]) + (0.098 * bbuf[x]) + 16;;
            vv = (0.439 * rbuf[x]) - (0.368 * gbuf[x]) - (0.071 * bbuf[x]) + 128;
            uu = -(0.148 * rbuf[x]) - (0.291 * gbuf[x]) + (0.439 * bbuf[x]) + 128;
            fflush(stdout);
            picture->data[0][xy] = yy;
            
            if( (x % 2) && (y % 2) )
            {
                    picture->data[1][xy_uv] = uu;
                    picture->data[2][xy_uv] = vv;
                    xy_uv++;
            }

            xy++;
          }

        }
        free(pixbuf);

        /* encode the image */
        out_size = avcodec_encode_video(c, outbuf, DEFAULT_BUFFER_SIZE, picture);
        fwrite(outbuf, 1, out_size, fmpg);
#endif

        return (0);

}


/****************************************
 * close the mpeg, free buffer, and close file
****************************************/
int gsd_close_mpeg()
{
#ifdef HAVE_FFMPEG
int i;

        /* get the delayed frames */
        for(; out_size; i++) {
                fflush(stdout);
                out_size = avcodec_encode_video(c, outbuf, DEFAULT_BUFFER_SIZE, NULL);
                fprintf(stderr, "write frame %3d (size=%5d)\n", i, out_size);
                fwrite(outbuf, 1, out_size, fmpg);
        }

        /* add sequence end code to have a real mpeg file */
        outbuf[0] = 0x00;
        outbuf[1] = 0x00;
        outbuf[2] = 0x01;
        outbuf[3] = 0xb7;
        fwrite(outbuf, 1, 4, fmpg);
        
        fclose(fmpg);
        free(picture_buf);
        free(outbuf);

        avcodec_close(c);
        av_free(c);
        av_free(picture);

	fprintf(stderr, "Close MPEG stream\n");
#endif

        return (0);
}


