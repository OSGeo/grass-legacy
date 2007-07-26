#include <stdlib.h>
#include <stdio.h>

#include <grass/ogsf_proto.h>
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

/******************************************
 * initialize FAME setup mpeg defaults and
 * open file for writing
******************************************/
int gsd_init_mpeg(char *name)
{
#ifdef HAVE_FFMPEG
	GLuint l, r, b, t;
	GLint tmp[4];
	int bitrate = 400000;

        glGetIntegerv(GL_VIEWPORT, tmp);
        l = tmp[0];
        r = tmp[0] + tmp[2] - 1;
        b = tmp[1];
        t = tmp[1] + tmp[3] - 1;

	fprintf(stderr, "Opening MPEG stream <%s> ...\n", name);

        avcodec_init();
	avcodec_register_all();

#ifdef USE_XVID
	codec = avcodec_find_encoder(CODEC_ID_XVID);
	bitrate=2000000;
#else
        codec = avcodec_find_encoder(CODEC_ID_MPEG1VIDEO);
#endif

        if (!codec) {
                fprintf(stderr, "codec not found\n");
                return(-1);
        }

        c= avcodec_alloc_context();
        picture= avcodec_alloc_frame();

        /* put sample parameters */
        c->bit_rate = bitrate;
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
		 G_debug(1, "avcodec_open() returned %d", avcodec_open(c, codec));
                return(-1);
        }

        if (NULL == (fmpg = fopen(name, "wb"))) {
                fprintf(stderr, "Cannot open file for output.\n");
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
int gsd_write_mpegframe(void)
{
#ifdef HAVE_FFMPEG
        unsigned int xsize, ysize;
        int x, y, xy, xy_uv;
        int yy, uu, vv;
        unsigned char *pixbuf;

        gsd_getimage(&pixbuf, &xsize, &ysize);
        xy = xy_uv = 0;
        for (y = ysize - 1; y >= 0; y--) {
          for (x = 0; x < xsize; x++) {
	    unsigned char r = pixbuf[(y * xsize + x) * 4 + 0];
	    unsigned char g = pixbuf[(y * xsize + x) * 4 + 1];
	    unsigned char b = pixbuf[(y * xsize + x) * 4 + 2];

            yy = (0.257 * r) + (0.504 * g) + (0.098 * b) + 16;;
            vv = (0.439 * r) - (0.368 * g) - (0.071 * b) + 128;
            uu = -(0.148 * r) - (0.291 * g) + (0.439 * b) + 128;
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
int gsd_close_mpeg(void)
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

	fprintf(stderr, "Closed MPEG stream.\n");
#endif

        return (0);
}

