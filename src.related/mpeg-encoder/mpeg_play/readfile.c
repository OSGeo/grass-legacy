/*
 * readfile.c --
 *
 *       Procedures concerned with reading data and parsing 
 *       start codes from MPEG files.
 *
 */

/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include "video.h"
#include "proto.h"
#include <sys/types.h>
#include <signal.h>
#ifndef MIPS
#include <netinet/in.h>
#else
#include <bsd/netinet/in.h>
#endif

#include "util.h"
#include "dither.h"

/* 
  Are we a system layer parser or pure video?
  (-1 is uninit, 0 is video, 1 is sys_layer)
*/
int sys_layer = -1;
int audBytes,vidBytes,sysBytes;

/* End of File flag. */
extern int EOF_flag;

/* Global file pointer to incoming data. */
extern FILE *input;

/* Values to handle stream seeking */
extern int seekValue;
extern unsigned int *bitBuffer;
extern int bufLength;

/* Options to control logging */
extern FILE *syslogOutput;
extern int opts;

/* Silly Constants.... */
#define PACK_START_CODE             ((unsigned int)0x000001ba)
#define SYSTEM_HEADER_START_CODE    ((unsigned int)0x000001bb)
#define PACKET_START_CODE_MASK      ((unsigned int)0xffffff00)
#define PACKET_START_CODE_PREFIX    ((unsigned int)0x00000100)
#define ISO_11172_END_CODE          ((unsigned int)0x000001b9)
  
#define PACK_HEADER_SIZE 8
  
#define STD_AUDIO_STREAM_ID ((unsigned char) 0xb8)
#define STD_VIDEO_STREAM_ID ((unsigned char) 0xb9)
#define MIN_STREAM_ID_ID    ((unsigned char) 0xbc)
#define RESERVED_STREAM_ID  ((unsigned char) 0xbc)
#define PRIVATE_STREAM_1_ID ((unsigned char) 0xbd)
#define PADDING_STREAM_ID   ((unsigned char) 0xbe)
#define PRIVATE_STREAM_2_ID ((unsigned char) 0xbf)
  
#define STD_SYSTEM_CLOCK_FREQ (unsigned long)90000
#define MUX_RATE_SCALE_FACTOR 50
#define MAX_STREAMS 8
#define NOT_PACKET_ID       ((unsigned char) 0xff)
#define KILL_BUFFER         ((unsigned char) 0xfe)
  

/*
 *--------------------------------------------------------------
 *
 * get_more_data --
 *
 *	Called by get_more_data to read in more data from
 *      video MPG files (non-system-layer)
 *
 * Results:
 *	Input buffer updated, buffer length updated.
 *      Returns 1 if data read, 0 if EOF, -1 if error.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
int 
get_more_data(bs_ptr, max_length, length_ptr, buf_ptr)
      unsigned int **bs_ptr;
      int *max_length, *length_ptr;
      unsigned int **buf_ptr;
{
  static BOOLEAN swap;
  int ioBytes, data, result;
  unsigned int *mark;
  
  if (sys_layer == 0) {
    return pure_get_more_data(*bs_ptr, *max_length, length_ptr, buf_ptr, swap);
  }

  if (sys_layer == -1) {
    /* Time to init ourselves */
    swap = (htonl(1) != 1);
    mark = *bs_ptr;
    ioBytes = fread(&data, 1, 4, input);

    if (ioBytes != 4) {
	  return 0;
    }

    data = ntohl(data);
    if ( (data == PACK_START_CODE) || (data == SYSTEM_HEADER_START_CODE) ) {
      /* Yow, a System Layer Stream.  Much harder to parse.  Call in the
	     specialist.... */
      fprintf(stderr,"This is an MPEG System Layer Stream.  ");
      fprintf(stderr,"Audio is not played.\n");
      sys_layer = 1;
      result = read_sys(bs_ptr, max_length, length_ptr, buf_ptr, (unsigned int) data);
      return result;
    } else {
      /* No system Layer junk, just pretent we didn't peek,
	     and hereafter just call pure_get_more_data */
      sys_layer = 0;
      **bs_ptr = data;
      *length_ptr = 1;
      result = pure_get_more_data(*bs_ptr, *max_length, 
				 length_ptr, buf_ptr, swap);
      *buf_ptr = *bs_ptr;
      return result;
    }
  }

  /* A system layer stream (called after the 1st time), call the specialist */
  result = read_sys(bs_ptr, max_length, length_ptr, buf_ptr, 0);
  return result;
}


/*
 *-------------------------------------------------------------
 *
 * clear_data_stream
 *
 * Empties out internal buffers
 *
 *-------------------------------------------------------------
 */
void 
  clear_data_stream(bs_ptr, max_length, length_ptr, buf_ptr)
unsigned int **bs_ptr;
int *max_length, *length_ptr;
unsigned int **buf_ptr;
{
  /* Only internal buffer is in ReadPacket */
  if (sys_layer) {
    ReadPacket(KILL_BUFFER, bs_ptr, max_length, length_ptr, buf_ptr);
  }
}

/*
 *-------------------------------------------------------------
 *
 * SeekStream
 *
 * Goto an offset in the steam
 *
 *-------------------------------------------------------------
 */
void
  SeekStream(vid_stream)
VidStream *vid_stream;
{
  int errno;
  int code;
  
  if (seekValue < 0) return;
#ifdef SEEK_SET
  errno = fseek(input, seekValue, SEEK_SET);
#else
  errno = fseek(input, seekValue, 0);
#endif
  if (errno != 0) {
    fprintf(stderr,"Error in seek (%d)\n",errno);
    perror("mpeg_play");
  }
  seekValue = 0-seekValue;
  totNumFrames = 0;

  /* clear that buffer */
  bitBuffer = vid_stream->buffer = vid_stream->buf_start;
  bufLength = vid_stream->buf_length = 0;
  bitOffset = vid_stream->bit_offset = 0;

  /* Find a decent start code */
 restart:
 NO_ZEROS:
  switch(fgetc(input)) {
  case 0:    goto ONE_ZERO;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }
  
 ONE_ZERO:
  switch(fgetc(input)) {
  case 0:    goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }
  
 TWO_ZEROS:
  switch(fgetc(input)) {
  case 0x01:  goto CODE_FOUND;
  case 0x00:  goto TWO_ZEROS;
  case EOF:   goto EOF_FOUND;
  default:    goto NO_ZEROS;
  }
  
 CODE_FOUND:
  code = 0x00000100+fgetc(input);
  if (sys_layer) {
    clear_data_stream(&vid_stream->buf_start,
		      &vid_stream->max_buf_length,
		      &vid_stream->buf_length, 
		      &vid_stream->buffer);
    if (((code & PACKET_START_CODE_MASK) == PACKET_START_CODE_PREFIX) &&
	((code & 0xff) >= 0xbc)) {
      read_sys(&vid_stream->buf_start,
	       &vid_stream->max_buf_length,
	       &vid_stream->buf_length, 
	       &vid_stream->buffer,
	       code);
      bufLength = vid_stream->buf_length;
      while (TRUE) {
	next_start_code();
	show_bits32(code);
	if ((code == SEQ_START_CODE) ||
	    (code == GOP_START_CODE)) return;
	flush_bits32; 
      }
    }
  } else {
    if ((code == SEQ_START_CODE) ||
	(code == GOP_START_CODE)) {
      *vid_stream->buffer = code;
      bufLength = vid_stream->buf_length = 1;
      return;
    }
  }
  goto restart;

 EOF_FOUND:   /* received EOF */
  fprintf(stderr, "Hit EOF after seeking (offset %ld)\n",ftell(input));
  exit(1);

}


/*
 *--------------------------------------------------------------
 *
 * pure_get_more_data --
 *      (get_more_data from ver 2.0 with swap added)
 *
 *	Called by get_more_data to read in more data from
 *      video MPG files (non-system-layer)
 *
 * Results:
 *	Input buffer updated, buffer length updated.
 *      Returns 1 if data read, 0 if EOF, -1 if error.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

int 
pure_get_more_data(buf_start, max_length, length_ptr, buf_ptr, swap)
     unsigned int *buf_start;
     int max_length;
     int *length_ptr;
     unsigned int **buf_ptr;
     BOOLEAN swap;
{
  
  int length, num_read, i;
  unsigned int request;
  unsigned char *buffer, *mark;
  unsigned int *lmark;
  
  if (EOF_flag) return 0;
  
  length = *length_ptr;
  buffer = (unsigned char *) *buf_ptr;
  
  if (length > 0) {
    memcpy((unsigned char *) buf_start, buffer, (unsigned int) (length*4));
    mark = ((unsigned char *) (buf_start + length));
  }
  else {
    mark = (unsigned char *) buf_start;
    length = 0;
  }
  
  request = (max_length-length)*4;
  
  
  num_read = fread(mark, 1, request, input);
  
  /* Paulo Villegas - 26/1/1993: Correction for 4-byte alignment */
  {
    int num_read_rounded;
    unsigned char *index;
    
    num_read_rounded = 4*(num_read/4);
    
    /* this can happen only if num_read<request; i.e. end of file reached */
    if ( num_read_rounded < num_read ) { 
 	  num_read_rounded = 4*( num_read/4+1 );

 	    /* fill in with zeros */
 	  for( index=mark+num_read; index<mark+num_read_rounded; *(index++)=0 );

 	  /* advance to the next 4-byte boundary */
 	  num_read = num_read_rounded;
    }
  }
  
  if (num_read < 0) {
    return -1;
  } else if (num_read == 0) {
    *buf_ptr = buf_start;
    
    /* Make 32 bits after end equal to 0 and 32
     * bits after that equal to seq end code
     * in order to prevent messy data from infinite
     * recursion.
     */
    
    *(buf_start + length) = 0x0;
    *(buf_start + length+1) = SEQ_END_CODE;
    
    EOF_flag = 1;
    return 0;
  }
  
  lmark = (unsigned int *) mark;
  
  num_read = num_read/4;
  
  if (swap) {
    for (i = 0; i < num_read; i++) {
      *lmark = htonl(*lmark);
      lmark++;
    }
  }
  
  *buf_ptr = buf_start;
  *length_ptr = length + num_read;
  
  return 1;
}




/* 
  Here is the specialist.... 
  Code is adapted from our program demux....
  A bunch of this needs to be #ifdef ANALYSIS'ed
  define __SYSREAD_LOGGING_ON__ to get  an output file for debugging
  */


/* Stream IDs */
static int gAudioStreamID;
static int gVideoStreamID;
static int gReservedStreamID;

#ifdef ANALYSIS
/* Statistics */
static int gNumAudioPackets;
static int gNumVideoPackets;
static int gNumPaddingPackets;
static int gNumReservedPackets;
static int gNumPrivate_1_Packets;
static int gNumPrivate_2_Packets;
#endif

/*
 *----------------------------------------------------------
 *
 *  read_sys
 *
 *      Parse out a packet of the system layer MPEG file.
 *
 *  Results:  Returns 0 if error or EOF
 *            Returns 1 if more data read (could be just one int)
 *
 *  Side Effects:  ReadPacket can change *bs_ptr to be a new buffer
 *                 buf_ptr will remain pointing at *length_ptr (at input)
 *                         into the buffer
 *                 *length_ptr will be changed to the new size
 *                 *max_length can be changed if a new buffer is alloc'd
 *
 *----------------------------------------------------------
 */
int read_sys(bs_ptr, max_length, length_ptr, buf_ptr, start)
     unsigned int **bs_ptr;
     int *max_length, *length_ptr;
     unsigned int **buf_ptr, start;  
     /* start is either a start code or 0 to indicate continued parsing */
{
  unsigned int startCode;
  int errorCode, PacketReply;
  unsigned char packetID;
  double systemClockTime;
  unsigned long muxRate;
  /* Statistics */
  static int numPacks = 0;
  static int numPackets = 0;
  static int numSystemHeaders = 0;
  static BOOLEAN Parse_done=FALSE;
  BOOLEAN match;
  
  if (!start) {
    errorCode = ReadStartCode(&startCode);
    if (EOF_flag) return 0;
    if (errorCode != 0) {
      fprintf(stderr, "Unable to read initial pack start code\n");
      return 0;
    }}
  else {
    errorCode = 0;
    startCode = start;
  }
  
  while (1) {
    match=FALSE;
    if (startCode == PACK_START_CODE) {
      ++numPacks; 
      match = TRUE;
      errorCode = ReadPackHeader( &systemClockTime, &muxRate);
      if (errorCode != 0) {
        fprintf(stderr, "Error in reading pack header\n");
        return 0;
      }
      errorCode = ReadStartCode( &startCode);
      if (errorCode != 0) {
        fprintf(stderr, "Error in reading start code\n");
        return 0;
      }
    }
    if (startCode == SYSTEM_HEADER_START_CODE) {
      ++numSystemHeaders; 
      match = TRUE;
      errorCode = ReadSystemHeader();
      if (errorCode != 0) {
        fprintf(stderr, "Error in reading system header\n");
        return 0;
      }
      errorCode = ReadStartCode( &startCode);
      if (errorCode != 0) {
        fprintf(stderr,"Error in reading start code after system header\n");
        return 0;
      }
    }
    packetID = startCode & 0xff;
    while (((startCode & PACKET_START_CODE_MASK) == PACKET_START_CODE_PREFIX) &&
	   (packetID >= 0xbc)) {
      ++numPackets; 
      match = TRUE;
      packetID = startCode & 0xff;
      PacketReply = ReadPacket(packetID, bs_ptr, max_length, length_ptr, buf_ptr);
      switch (PacketReply) {
      case 2: 
        return 1;
      case 1: 
        return 0;
      default: /* do nothing */
        break;
      }
      errorCode = ReadStartCode( &startCode);
      if (errorCode != 0) {
        fprintf(stderr,"Error in start code after packet\n");
        return 0;
      }
      if (startCode == PACK_START_CODE || startCode == ISO_11172_END_CODE) {
        break;
      }
    }
    
    if (startCode == ISO_11172_END_CODE) {
      match = TRUE;
      if (Parse_done) {
	return 1;
      }
#ifdef ANALYSIS
      fprintf(stderr, "Successful parse of MPEG system level\n");
      fprintf(stderr, "%d system headers, %d packs, %d packets\n",
	      numSystemHeaders, numPacks, numPackets);
      fprintf(stderr, "%d audio packets, %d video packets, %d padding packets\n",
	      gNumAudioPackets, gNumVideoPackets, gNumPaddingPackets);
      fprintf(stderr, "%d reserved packets, %d/%d private type 1/2 packets\n",
	      gNumReservedPackets, gNumPrivate_1_Packets, gNumPrivate_2_Packets);
#endif
      ReadPacket(NOT_PACKET_ID, bs_ptr, max_length, length_ptr, buf_ptr);
      Parse_done = TRUE;
      return 1;
    }
    if (errorCode != 0)
      return 1;
    if (! match) {
      fprintf(stderr,"\nNo match found for start code %08x in system layer, skipping\n",startCode);
      startCode = find_start_code();
      if (startCode == EOF) {
        EOF_flag = 1;
        return 0;
      }
    }
  }
}


/*
 *-----------------------------------------------------------
 *
 *  ReadStartCode
 *
 *      Parses a start code out of the stream
 *
 *  Results/Side Effects:  Sets *startCode to the code, returns
 *     1 on error, 0 on success
 *
 *-----------------------------------------------------------
 */
int ReadStartCode(startCode)
     unsigned int *startCode;
{
  int numRead;
  
  numRead = fread((unsigned char *)startCode, 1, 4, input);
  *startCode = htonl(*startCode);
  
  if (numRead < 4) {
    EOF_flag = 1;
    return 1;
  }

  if ((*startCode&0xfffffe00) != 0) {
    fprintf(stderr,"Problem with system layer parse, skipping to start code\n");
    *startCode = find_start_code();
    if (*startCode == EOF) {
      EOF_flag = TRUE;
      return 0;
    }
  }

  sysBytes += 4;
  return 0;
}


/*
 *-----------------------------------------------------------
 *
 *  find_start_code
 *
 *      Parses a start code out of the stream by tossing bytes until it gets one
 *
 *  Results/Side Effects:  Parses bytes of the stream, returns code
 *                         Returns EOF in case of end of file
 *
 *-----------------------------------------------------------
 */
int find_start_code()
{
 NO_ZEROS:
  switch(fgetc(input)) {
  case 0:    goto ONE_ZERO;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }

 ONE_ZERO:
  switch(fgetc(input)) {
  case 0:    goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }

 TWO_ZEROS:
  switch(fgetc(input)) {
  case 0x01:  goto CODE_FOUND;
  case 0x00:  goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:    goto NO_ZEROS;
  }

 CODE_FOUND:
  return 0x00000100+fgetc(input);

 EOF_FOUND:   /* received EOF */
  return EOF;
}




/*
 *-----------------------------------------------------------------
 *
 *  ReadPackHeader
 *
 *      Parses out the PACK header
 *
 *  Returns: 1 on error, 0 on success
 *
 *-------------------------------------------------------------------
 */
int ReadPackHeader(systemClockTime,muxRate)
     double *systemClockTime;
     unsigned long *muxRate;
{
  int numRead;
  unsigned char inputBuffer[PACK_HEADER_SIZE];
  unsigned long systemClockRef;
  unsigned char systemClockRefHiBit;
  int errorCode;
  
  numRead = fread(inputBuffer, 1, PACK_HEADER_SIZE, input);
  if (numRead < PACK_HEADER_SIZE) {
    EOF_flag = 1;
    return 1;
  }
  sysBytes += numRead;
  ReadTimeStamp(inputBuffer, &systemClockRefHiBit, &systemClockRef);
  errorCode = MakeFloatClockTime(systemClockRefHiBit, systemClockRef, 
				 systemClockTime);
  ReadRate(&inputBuffer[5], muxRate);
  *muxRate *= MUX_RATE_SCALE_FACTOR;
  return 0;
}


/*
 *------------------------------------------------------------------
 *
 *   ReadSystemHeader
 *
 *      Parse out the system header, setup out stream IDs for parsing packets
 *
 *   Results:  Returns 1 on error, 0 on success.
 *             Sets gAudioStreamID and gVideoStreamID
 *
 *------------------------------------------------------------------
 */
int ReadSystemHeader()
{ 
  unsigned char *inputBuffer = NULL;
  int numRead;
  int pos;
  unsigned short headerSize;
  unsigned char streamID;
  
  numRead = fread((char *)&headerSize, 1, 2, input); 
  headerSize = ntohs(headerSize);
  if (numRead != 2) {
    EOF_flag = 1;
    return 1;
  }
  inputBuffer = (unsigned char *) malloc((unsigned int) headerSize+1);
  sysBytes += headerSize;
  if (inputBuffer == NULL) {
    return 1;
  }
  inputBuffer[headerSize]=0;
  numRead = fread(inputBuffer, 1, headerSize, input); 
  if (numRead < headerSize) {
    EOF_flag = 1;
    return 1;
  }
  
  pos = 6;
  while ((inputBuffer[pos] & 0x80) == 0x80) {
    streamID = inputBuffer[pos];
    switch (streamID) {
    case STD_VIDEO_STREAM_ID: 
      break;
    case STD_AUDIO_STREAM_ID: 
      break;
    case RESERVED_STREAM_ID: 
      break;
    case PADDING_STREAM_ID: 
      break;
    case PRIVATE_STREAM_1_ID: 
      break;
    case PRIVATE_STREAM_2_ID: 
      break;
    default:
      if (streamID < MIN_STREAM_ID_ID) {
	return 1;
      }
      switch (streamID >> 4) {
      case 0xc:
      case 0xd:
	gAudioStreamID = streamID;
	break;
      case 0xe:
	if ((gVideoStreamID != 0) && (gVideoStreamID!=streamID)) {
	  break;
	}
	gVideoStreamID = streamID;
	break;
      case 0xf:
	gReservedStreamID = streamID;
	break;
      }
      break;
    }
    pos += 3;
  }
  if (inputBuffer != NULL)
    free(inputBuffer);
  return 0;
}


/*
 *-----------------------------------------------------------------
 *
 *  ReadPacket
 *
 *      Reads a single packet out of the stream, and puts it in the
 *      buffer if it is video.
 *
 *  Results:
 *      Changes the value of *length_ptr to be the new length (plus old)
 *      If the buffer is too small, can change *bs_ptr, *max_length, and 
 *      buf_ptr to be correct for a newly allocated buffer.
 *
 *  State:  
 *      The buffer is in ints, but the packets can be an arbitrary number
 *      of bytes, so leftover bytes are kept in static vars and added in 
 *      on the next call.
 *
 *-----------------------------------------------------------------
 */   
int ReadPacket(packetID, bs_ptr, max_length, length_ptr, buf_ptr) 
     unsigned char packetID;
     unsigned int **bs_ptr;
     int *max_length;
     int *length_ptr;
     unsigned int **buf_ptr;
     /* Returns:
	0 - no error, but not video packet we want
	1 - error
	2 - got video packet into buffer
	*/
{   
  int ioBytes;
  unsigned char nextByte;
  unsigned short packetLength;
  unsigned char *packetBuffer = NULL;
  int pos;
  int numStuffBytes = 0;
  unsigned int packetDataLength;
  int byte_length;
  unsigned char scratch[9];
  /* Leftovers from previous video packets */
  static unsigned int num_left = 0, leftover_bytes = 0;
  
  if (packetID == NOT_PACKET_ID) {
    /* Gross hack to handle unread bytes before end of stream */
    if (num_left != 0) {
      /* Sigh, deal with previous leftovers */
      *(*buf_ptr+*length_ptr) = leftover_bytes;
      *(*buf_ptr+*length_ptr+1) = ISO_11172_END_CODE;
      *length_ptr += 2;
    } else {
      *(*buf_ptr+*length_ptr) = ISO_11172_END_CODE;
      *length_ptr += 1;
    }
    return 1;
  } else if (packetID==KILL_BUFFER) {
    num_left=0;
    leftover_bytes=0;
    return 0;
  }
  
  ioBytes = fread(&packetLength, 1, 2, input);
  packetLength = htons(packetLength);
  if (ioBytes < 2) {
    return 1;
  }
  if (packetID == gAudioStreamID) {
#ifdef ANALYSIS
    ++gNumAudioPackets;
#endif
  }
  else if (packetID == gVideoStreamID) {
#ifdef ANALYSIS     
    ++gNumVideoPackets;
#endif
  }
  else {
    switch (packetID) {
    case PADDING_STREAM_ID:
#ifdef ANALYSIS
      ++gNumPaddingPackets;
#endif
      break;
    case RESERVED_STREAM_ID:
#ifdef ANALYSIS
      ++gNumReservedPackets;
#endif
      break;
    case PRIVATE_STREAM_1_ID:
#ifdef ANALYSIS
      ++gNumPrivate_1_Packets;
#endif
      break;
    case PRIVATE_STREAM_2_ID:
#ifdef ANALYSIS
      ++gNumPrivate_2_Packets;
#endif
      break;
    default:
      fprintf(stderr, "\nUnknown packet type encountered. P'bly audio? (%x) at %d\n",
	      packetID,(int) ftell(input));
    }
    fseek(input, packetLength, 1);
    sysBytes += packetLength;
    return 0;
  }
  fread(&nextByte,1,1,input);
  pos = 0;
  while (nextByte & 0x80) {
    ++numStuffBytes;
    ++pos;
    fread(&nextByte,1,1,input);
  }
  if ((nextByte >> 6) == 0x01) {
    pos += 2;
    fread(&nextByte,1,1,input);
    fread(&nextByte,1,1,input);
  } 
  if ((nextByte >> 4) == 0x02) {
    fread(scratch,1,4,input);
    fread(&nextByte,1,1,input);
    pos += 5;
  }
  else if ((nextByte >> 4) == 0x03) {
    fread(scratch,1,9,input);
    fread(&nextByte,1,1,input);
    pos += 10;
  } 
  else {
    fread(&nextByte,1,1,input);
    pos += 1;
  }
  /* Read all the headers, now make room for packet */
  if (*bs_ptr + *max_length < *buf_ptr+ packetLength/4 + *length_ptr) {
    if (*max_length - *length_ptr < packetLength/4) {
      /* Buffer too small for a packet (plus whats there), 
	   * time to enlarge it! 
	   */
      unsigned int *old = *bs_ptr;
      *max_length = *length_ptr + packetLength/2;
      *bs_ptr=(unsigned int *)malloc(*max_length*4);
      if (*bs_ptr == NULL) {
        return 1;
      }
      memcpy((unsigned char *)*bs_ptr, *buf_ptr, (unsigned int) *length_ptr*4);
      free(old);
      *buf_ptr = *bs_ptr;
    } else {
      memcpy((unsigned char *)*bs_ptr, *buf_ptr, (unsigned int) *length_ptr*4);
      *buf_ptr = *bs_ptr;
    }}
  byte_length = *length_ptr*4;
  if (num_left != 0) {
    /* Sigh, deal with previous leftovers */
    byte_length += num_left;
    *(*buf_ptr+*length_ptr) = leftover_bytes;
  }
  packetBuffer=((unsigned char *)*buf_ptr)+byte_length;
  packetDataLength = packetLength - pos;
  *packetBuffer++ = nextByte;
  if (packetID == gVideoStreamID) {
    ioBytes = fread(packetBuffer, 1, packetDataLength-1, input);
    if (ioBytes != packetDataLength-1) {
      EOF_flag = 1;
      return 1;
    }
    if (1 != ntohl(1)) {
      unsigned int *mark = *buf_ptr+*length_ptr;
      int i;
      
      for (i=0; i < ((packetDataLength+num_left)&0xfffffffc); i+=4) {
        *mark=ntohl(*mark);
        mark++;
      }
    }
    byte_length = byte_length + packetDataLength;
    num_left = byte_length % 4;
    *length_ptr = byte_length / 4;
    leftover_bytes = *(*buf_ptr + *length_ptr);
    sysBytes += packetLength - packetDataLength;
    vidBytes += packetDataLength;
    return 2;
  }
  else if (packetID == gAudioStreamID) { 
    sysBytes += packetLength - packetDataLength;
    audBytes += packetDataLength;
    packetBuffer = (unsigned char *)(*buf_ptr + *length_ptr + 1);
    fread(packetBuffer, 1, packetDataLength - 1, input);
  }
  else /* Donno what it is, just nuke it */ {
    /* This code should be unreachable */
    sysBytes += packetLength;
    packetBuffer = (unsigned char *)(*buf_ptr + *length_ptr + 1);
    fread(packetBuffer, 1, packetDataLength - 1, input);
  }
  return 0; 
}


/*
 * The remaining procedures are formatting utility procedures.
 */
void ReadTimeStamp(inputBuffer,hiBit,low4Bytes)
     unsigned char *inputBuffer, *hiBit;
     unsigned long *low4Bytes;
{
  *hiBit = ((unsigned long)inputBuffer[0] >> 3) & 0x01;
  *low4Bytes = (((unsigned long)inputBuffer[0] >> 1) & 0x03) << 30; 
  *low4Bytes |= (unsigned long)inputBuffer[1] << 22; 
  *low4Bytes |= ((unsigned long)inputBuffer[2] >> 1) << 15; 
  *low4Bytes |= (unsigned long)inputBuffer[3] << 7; 
  *low4Bytes |= ((unsigned long)inputBuffer[4]) >> 1; 
}

void ReadSTD(inputBuffer,stdBufferScale,stdBufferSize) 
unsigned char *inputBuffer;
unsigned char *stdBufferScale;
unsigned long *stdBufferSize;
{
  *stdBufferScale = ((inputBuffer[0] & 0x20) >> 5); 
  *stdBufferSize = ((unsigned long)inputBuffer[0] & 0x1f) << 8;
  *stdBufferSize |= (unsigned long)inputBuffer[1];
}


void ReadRate(inputBuffer,rate)
     unsigned char *inputBuffer;
     unsigned long *rate;
{
  *rate = (inputBuffer[0] & 0x7f) << 15;
  *rate |= inputBuffer[1] << 7;
  *rate |= (inputBuffer[2] & 0xfe) >> 1;
}

#define FLOAT_0x10000 (double)((unsigned long)1 << 16)

int MakeFloatClockTime(hiBit,low4Bytes,floatClockTime)
     unsigned char hiBit;
     unsigned long low4Bytes;
     double *floatClockTime;
{
  if (hiBit != 0 && hiBit != 1) {
    *floatClockTime = 0.0;
    return 1;
  }
  *floatClockTime 
    = (double)hiBit*FLOAT_0x10000*FLOAT_0x10000 + (double)low4Bytes;
  *floatClockTime /= (double)STD_SYSTEM_CLOCK_FREQ;
  return 0;
}
