/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA NOR BOSTON UNIVERSITY
 * BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA AND BOSTON UNIVERSITY
 * SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */
/************************************************************************
*                     jmovie2jpeg.c                                     *
*                   By James Boucher(jboucher@flash.bu.edu)             *
*               Boston University Multimedia Communications Lab         *
* This code was adapted from the Berkeley Playone.c and Brian Smith's   *
* JGetFrame code after being modified on 10-7-93 by Dinesh Venkatesh    *
* of BU.                                                                *
*       This code converts a version 2 Parallax J_Movie into a          *
* set of JFIF compatible JPEG images. It works for all image            *
* sizes and qualities.                                                  *
************************************************************************/

#include <stdio.h>

#define HEADER_SIZE 607   /*JFIF header size used on output images*/

usage (p)
char *p;

{
    fprintf (stderr, "Usage: %s infilename outfilename start-frame-no end-frame-no\n", p);
    exit (1);
}

main (argc, argv)
int argc;
char **argv;
{
    char *infilename;     /* input filename string */
    char *obase;          /* output filename base string=>obase##.jpg */
    FILE *inFile;         /* Jmovie file pointer */
    FILE *outFile;        /* JPEG file pointer for output file */
    FILE *audiooutfile;   /* SPARC .AU audio output file pointer*/
    extern char *malloc();
    int fd, i;            /* input file descriptor and a counting variable*/
    int start, end;       /* first and last frames to be extracted */
    char ofname[256];     /* output filename string */
    int Temp = 0, temp = 0;  /* dummy variables */
    int image_offset = 0;    /* counting variable */
  /* J_Movie header infomation */
    int ver_no;           /* version number - expected to be 2 */
    int fps;              /* frame rate - frames per second */
    int no_frames;        /* total number of frames in jmovie */
    int bandwidth;        /* bandwidth required for normal playback*/
    int qfactor;          /* quality factor used to scale Q matrix */
    int mapsize;          /* number of color map entries - 2^24 */
    int audio_tracks;     /* number of audio tracks ==1    */
    int audiosize;        /*number of bytes in audio tracks */
    int *inoffsets;       /* input frame offsets from start of jmovie*/
    int width;            /* image width */
    int height;           /* image height */
    int size;             /* total image size in bytes */
    int volbase;          /* Baseline volume control */
    char op_code;         /* jmovie op_code */
    char jpeg_size[4];    /* jpeg data size */
    unsigned long audio_length; /* total length of audio data*/
static char audio[1000] = {
     0x2E, 0x73, 0x6E, 0x64, 0x00, 0x00, 0x00, 0x18,
     0x00, 0x00, 0x40, 0xaa, 0x00, 0x00, 0x00, 0x01,
     0x00, 0x00, 0x1F, 0x40, 0x00, 0x00, 0x00, 0x01};

/* The next array represents the default JFIF header for
quality = 100 and size = 320x240. The values are
adjusted as the J_Movie header is read.  The default
psize of this array is set large so as to make room
for the appending of the jpeg bitstream. It can be
made smaller if you have a better idea of its expected size*/
static char inbuffer[300000] = {    
     0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46,  
     0x49, 0x46, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01,
     0x00, 0x01, 0x00, 0x00, 0xFF, 0xC0, 0x00, 0x11,  
     0x08, 0x00, 0xF0, 0x01, 0x40, 0x03, 0x01, 0x21,
     0x00, 0x02, 0x11, 0x01, 0x03, 0x11, 0x01, 0xFF,  
     0xDB, 0x00, 0x84, 0x00, 0x10, 0x0B, 0x0C, 0x0E,
     0x0C, 0x0A, 0x10, 0x0E, 0x0D, 0x0E, 0x12,  
     0x11, 0x10, 0x13, 0x18, 0x28, 0x1A, 0x18, 0x16,
     0x16, 0x18, 0x31, 0x23, 0x25, 0x1D, 0x28, 0x3A,  
     0x33, 0x3D, 0x3C, 0x39, 0x33, 0x38, 0x37, 0x40,
     0x48, 0x5C, 0x4E, 0x40, 0x44, 0x57, 0x45, 0x37,  
     0x38, 0x50, 0x6D, 0x51, 0x57, 0x5F, 0x62, 0x67,
     0x68, 0x67, 0x3E, 0x4D, 0x71, 0x79, 0x70, 0x64,  
     0x78, 0x5C, 0x65, 0x67, 0x63, 0x01, 0x11, 0x12,
     0x12, 0x18, 0x15, 0x18, 0x2F, 0x1A, 0x1A, 0x2F,  
     0x63, 0x42, 0x38, 0x42, 0x63, 0x63, 0x63, 0x63,
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0xFF, 0xC4,
     0x01, 0xA2, 0x00, 0x00, 0x01, 0x05, 0x01, 0x01,  
     0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00,
     0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x03, 0x04,  
     0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B,
     0x10, 0x00, 0x02, 0x01, 0x03, 0x03, 0x02, 0x04,  
     0x03, 0x05, 0x05, 0x04, 0x04, 0x00, 0x00, 0x01,
     0x7D, 0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05,  
     0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61,
     0x07, 0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xA1,  
     0x08, 0x23, 0x42, 0xB1, 0xC1, 0x15, 0x52, 0xD1,
     0xF0, 0x24, 0x33, 0x62, 0x72, 0x82, 0x09,   
     0x0A, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x25, 0x26,
     0x27, 0x28, 0x29, 0x2A, 0x34, 0x35, 0x36, 0x37,  
     0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47,
     0x48, 0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57,  
     0x58, 0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67,
     0x68, 0x69, 0x6A, 0x73, 0x74, 0x75, 0x76, 0x77,  
     0x78, 0x79, 0x7A, 0x83, 0x84, 0x85, 0x86, 0x87,
     0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95, 0x96,  
     0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5,
     0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4,  
     0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3,
     0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2,  
     0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA,
     0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8,  
     0xE9, 0xEA, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6,
     0xF7, 0xF8, 0xF9, 0xFA, 0x01, 0x00, 0x03, 0x01,  
     0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02,  
     0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 
     0x0A, 0x0B, 0x11, 0x00, 0x02, 0x01, 0x02, 0x04,  
     0x04, 0x03, 0x04, 0x07, 0x05, 0x04, 0x04, 0x00,
     0x01, 0x02, 0x77, 0x00, 0x01, 0x02, 0x03, 0x11,  
     0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51,
     0x07, 0x61, 0x71, 0x13, 0x22, 0x32, 0x81, 0x08,  
     0x14, 0x42, 0x91, 0xA1, 0xB1, 0xC1, 0x09, 0x23,
     0x33, 0x52, 0xF0, 0x15, 0x62, 0x72, 0xD1,   
     0x0A, 0x16, 0x24, 0x34, 0xE1, 0x25, 0xF1, 0x17,
     0x18, 0x19, 0x1A, 0x26, 0x27, 0x28, 0x29, 0x2A,  
     0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44,
     0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x53, 0x54,  
     0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x63, 0x64,
     0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73, 0x74,  
     0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x82, 0x83,
     0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x92,  
     0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A,
     0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9,  
     0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8,
     0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,  
     0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
     0xD7, 0xD8, 0xD9, 0xDA, 0xE2, 0xE3, 0xE4, 0xE5,  
     0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF2, 0xF3, 0xF4,
     0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFF, 0xDA,  
     0x00, 0x0C, 0x03, 0x01, 0x00, 0x02, 0x11, 0x03,
     0x11, 0x00, 0x3F, 0x00  

    };
      /* read command line data */    
    if (argc != 5) usage(argv[0]); 
    infilename = argv[1];
    obase  = argv[2];
    start = atoi(argv[3]);
    end = atoi(argv[4]);
    
      /* error checking */
    if (start < 1)
    {
	fprintf(stderr, "ERROR: start frame number must be at least 1.\n");
	exit(1);
    }

    if (start > end)
    {
	fprintf(stderr, "ERROR: start frame number greater than end.\n");
	exit(1);
    }
    
     /* open J_Movie */
    inFile = fopen(infilename, "r");
    if (inFile == NULL) 
    {
	perror (infilename);
	exit (1);
    }
    
   /* get file descriptor */    
    fd = fileno(inFile);
    
/* The following lines parse the jpeg_movie header and recover the */
/* relevant information */

    fseek (inFile, (8*sizeof(char)),0);
    
    if (fread (&ver_no,sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading version");
	exit(1);
    }  
    printf("Version is: %d\n",ver_no);
    if(ver_no != 2){
       perror("Unrecognized version - Only reads version 2 files\n");
       exit(1);
     }
    if (fread (&(fps),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading fps");
	exit(1);
    }  
    printf("Frames per sec is: %d\n",fps);
    if (fread (&(no_frames),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading no_frames");
	exit(1);
    }  
    printf("Number of Frames is: %d\n",no_frames - 1);

    inoffsets = (int *)malloc(no_frames*sizeof(int));
    
    if (fread (&(width),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading width");
	exit(1);
    }  
     /* set image width in JFIF header */
    inbuffer[27] = (char)(0xFF & (width >> 8));
    inbuffer[28] = (char)(0xFF & width);
 
    if (fread (&(height),sizeof(int), 1,inFile) != 1)
    {
	perror("Error in reading height");
	exit(1);
    }  
     /* set image height in JFIF header */
    inbuffer[25] = (char)(0xFF & (height >> 8));
    inbuffer[26] = (char)(0xFF & height);
    
    if (fread (&(bandwidth),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading bandwidth");
	exit(1);
    }  
    printf("Bandwidth required is : %d\n",bandwidth);
    
    if (fread (&(qfactor),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading qfactor");
	exit(1);
    }  
     /* The default quality factor = 100, therefore, if
        our quality factor does not equal 100 we must
        scale the quantization matrices in the JFIF header*/    
    /* Note values are clipped to a max of 255 */
    printf("Q-factor: %d\n",qfactor);
    if(qfactor != 100){
      for(Temp=44;Temp<108;Temp++){
        temp= (inbuffer[Temp]*qfactor)/100;
      inbuffer[Temp] = (char)((temp<255) ? temp : 255);
      }
      for(Temp=109;Temp<173;Temp++){
        temp = (inbuffer[Temp]*qfactor)/100;
      inbuffer[Temp] = (char)((temp<255) ? temp : 255);
      }    
    }
  
    if (fread (&(mapsize),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading mapsize");
	exit(1);
    }  
    printf("Number of color map entries allowed(2^24): %d\n",mapsize);
    if (fread (&(image_offset),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading image offset");
	exit();
    }
    if (fread (&(audio_tracks),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading audio tracks");
	exit();
    }
    if (audio_tracks != 1) 
      { 
	perror("Only expects one audio track\n");
        exit(1);
      }
    printf("No audio tracks = %d\n",audio_tracks);

    if( fread(&(volbase),sizeof(int),1,inFile) != 1)
    {
        perror("Error in reading baseline volume control");
	exit(1);
      }
    printf("Baseline volume control = %d\n", volbase);
    
    if (fread (&(audiosize),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading audiosize");
	exit(1);
    }
    printf("No. audio_bytes/frame = %d\n",audiosize);
    
    audio_length = audiosize *(end - start + 1);
    audio[8] = (char)(0xFF & (audio_length >>24));
    audio[9] = (char)(0xFF & (audio_length >>16));
    audio[10] = (char)(0xFF & (audio_length >> 8));
    audio[11] = (char)(0xFF & (audio_length));

    fseek (inFile,(image_offset),0);
    
    
    printf("Image width: %d, Image height: %d,Image offset: %d \n",width, height,image_offset);
    

    if(no_frames <= end)
    {
	printf("End greater than total frames in sequence \n");
	printf("Thats ok, setting end = %d \n",no_frames - 1);
	end = no_frames - 1;
    }
    

    for(i=0;i<no_frames;i++) 
    {
	fread(&(inoffsets[i]),sizeof(int),1,inFile);
    }/* Reads in the frame sizes into the array */
    
    rewind(inFile);
    sprintf(ofname, "%s.au",obase);
    audiooutfile = fopen(ofname, "w"); /* open audiofile*/
    fwrite(audio,24, sizeof(char),audiooutfile);

    /* Extract JFIF files from J_Movie */    
    for (i=start; i<=end ; i++) 
    {
	printf("GRABBING FRAME %d\n", i);
	
	size = inoffsets[i]- inoffsets[i-1]- 5;
	lseek(fd, inoffsets[i-1],0); 
	
	read(fd, &(op_code), 1);
        printf("frame size is %d\n",size); 
	if( op_code !=  0xffffffec)
	{
	    read(fd,audio,audiosize);
            fwrite(audio,audiosize, sizeof(char),audiooutfile);
            size = size -audiosize;
            read(fd,&(op_code),1); /* reads image op code*/
	}/* To skip the audio bytes in each frame */
	read(fd,jpeg_size,4);
	read(fd,&(inbuffer[607]),(size));
	sprintf(ofname,"%s%d.jpg",obase,i);
	outFile = fopen(ofname, "w");
	fwrite(inbuffer,(size+607),sizeof(char),outFile);
	fclose(outFile);        

    }

    free(inoffsets);
    fclose(inFile);
    fclose(audiooutfile);
    
}




