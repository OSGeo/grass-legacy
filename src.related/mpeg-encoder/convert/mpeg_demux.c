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

#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/param.h>
#include <netinet/in.h>  /* htonl, htons, ntohs */

#define __LOGGING_ON__

#define PACK_START_CODE             (unsigned int)0x000001ba
#define SYSTEM_HEADER_START_CODE    (unsigned int)0x000001bb
#define PACKET_START_CODE_MASK      (unsigned int)0xffffff00
#define PACKET_START_CODE_PREFIX    (unsigned int)0x00000100
#define ISO_11172_END_CODE          (unsigned int)0x000001b9

#define PACK_HEADER_SIZE 8

#define STD_AUDIO_STREAM_ID 0xb8
#define STD_VIDEO_STREAM_ID 0xb9
#define MIN_STREAM_ID_ID    0xbc
#define RESERVED_STREAM_ID  0xbc
#define PRIVATE_STREAM_1_ID 0xbd
#define PADDING_STREAM_ID   0xbe
#define PRIVATE_STREAM_2_ID 0xbf

#define STD_SYSTEM_CLOCK_FREQ (unsigned long)90000
#define MUX_RATE_SCALE_FACTOR 50

#define MAX_STREAMS 8

int ReadStartCode(
   FILE *inputStream,
   unsigned int *startCode);

int ReadPackHeader(
   FILE *inputStream,
   double *systemClockTime,
   unsigned long *muxRate);

int ReadSystemHeader(
   FILE *inputStream);

int ReadPacket(
   unsigned char packetID,
   FILE *inputStream,
   FILE *videoOutputStream,
   FILE *audioOutputStream,
   FILE *packetInfoFile);

void ReadTimeStamp(
   unsigned char *inputBuffer,
   unsigned char *hiBit,
   unsigned long *low4Bytes);

void ReadSTD(
   unsigned char *inputBuffer,
   unsigned char *stdBufferScale,
   unsigned long *stdBufferSize);

void ReadRate(
   unsigned char *inputBuffer,
   unsigned long *rate);

int MakeFloatClockTime(
   unsigned char hiBit,
   unsigned long low4Bytes,
   double *floatClockTime);

void PrintUsage(
   char *programName);

void ErrorExit(
   char *errorMessage); 

static int gStreamID[MAX_STREAMS];
static int gNumStreams = 0;
static int gAudioStreamID;
static int gVideoStreamID;
static int gReservedStreamID;
static int gNumAudioPackets;
static int gNumVideoPackets;
static int gNumPaddingPackets;
static int gNumReservedPackets;
static int gNumPrivate_1_Packets;
static int gNumPrivate_2_Packets;

#ifdef __LOGGING_ON__
static FILE *errorOutput;
#endif

main(int argc, char **argv)
  {
   unsigned int startCode;
   int errorCode;
   unsigned char packetID;
   int numPacks = 0;
   int numPackets = 0;
   int numSystemHeaders = 0;
   int pathStringLength;

   FILE *inputStream;
   FILE *videoOutputStream;
   FILE *audioOutputStream;
   FILE *packetInfoFile;

   char inputFileName[MAXPATHLEN + 1];
   char audioFileName[MAXPATHLEN + 1];
   char videoFileName[MAXPATHLEN + 1];
   char packetInfoFileName[MAXPATHLEN + 1];
   char errorString[64 + MAXPATHLEN];

   int packetsInPack;
   double systemClockTime;
   unsigned long muxRate;
  
   if (argc != 2) {
      PrintUsage(argv[0]);
      exit(0);
     }
   pathStringLength = strlen(argv[1]);
   if (pathStringLength > MAXPATHLEN)
      ErrorExit("Input file path too long");
  
   strcpy(inputFileName, argv[1]);
   if (!strcmp(inputFileName + pathStringLength - 4, ".mpg")) {
      strncpy(audioFileName, inputFileName, pathStringLength - 4);
      strcpy(audioFileName + pathStringLength - 4, ".aud");
      strncpy(videoFileName, inputFileName, pathStringLength - 4);
      strcpy(videoFileName + pathStringLength - 4, ".vid");
      strncpy(packetInfoFileName, inputFileName, pathStringLength - 4);
      strcpy(packetInfoFileName + pathStringLength - 4, ".sys");
     }
   else if (pathStringLength + 4 < MAXPATHLEN) {
      strncpy(audioFileName, inputFileName, pathStringLength);
      strcpy(audioFileName + pathStringLength, ".aud");
      strncpy(videoFileName, inputFileName, pathStringLength);
      strcpy(videoFileName + pathStringLength, ".vid");
      strncpy(packetInfoFileName, inputFileName, pathStringLength - 4);
      strcpy(packetInfoFileName + pathStringLength - 4, ".sys");
     } 
   else 
      ErrorExit("Input file path too long");

   inputStream = fopen(inputFileName, "r");
   if (inputStream == NULL) {
      sprintf(errorString, "Can't open input file %s\n", inputFileName);
      ErrorExit(errorString);
     }

   videoOutputStream = fopen(videoFileName, "w");
   if (videoOutputStream == NULL) {
      sprintf(errorString, "Can't open video output file %s\n", videoFileName);
      ErrorExit(errorString);
     }
   audioOutputStream = fopen(audioFileName, "w");
   if (audioOutputStream == NULL) {
      sprintf(errorString, "Can't open audio output file %s\n", audioFileName);
      ErrorExit(errorString);
     }

   packetInfoFile = fopen(packetInfoFileName, "w");
   if (packetInfoFile == NULL) {
      sprintf(errorString, "Can't open packet info output file %s\n", 
              packetInfoFileName);
      ErrorExit(errorString);
     }
   fprintf(packetInfoFile, "%9s|%8s|%7s|%4s|%15s|%10s|%9s|%9s\n",
           "System ", "Mux ", 
           "", "", "Bytes", "Buffer", "Present.", "Decoding");
   fprintf(packetInfoFile, "%9s|%8s|%7s|%4s|%15s|%10s|%9s|%9s\n",
           "Time  ", "Rate ",
           "Pkt. #", "Type", "Hdr(stuff)+data", "Size ", "Time  ", "Time  ");
   fprintf(packetInfoFile, "---------|");
   fprintf(packetInfoFile, "--------|");
   fprintf(packetInfoFile, "-------|");
   fprintf(packetInfoFile, "----|");
   fprintf(packetInfoFile, "---------------|");
   fprintf(packetInfoFile, "----------|");
   fprintf(packetInfoFile, "---------|");
   fprintf(packetInfoFile, "---------");
   fprintf(packetInfoFile, "\n");

#ifdef __LOGGING_ON__
   errorOutput = fopen("error.log", "w");
   if (errorOutput == NULL) {
      fprintf(stderr, "Can't open output file \"error.log\"\n");
      exit(1);
     }
/*
   errorOutput = stderr;
*/
#endif

   errorCode = ReadStartCode(inputStream, &startCode);
   if (errorCode != 0) {
      fprintf(stderr, "Unable to read initial pack start code\n");
      exit(1);
     }
   while (startCode == PACK_START_CODE) {
      ++numPacks;
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "PACK #%d:\n", numPacks);
#endif
      errorCode = ReadPackHeader(inputStream, &systemClockTime, &muxRate);
      if (errorCode != 0) {
         fprintf(stderr, "Error in reading pack header\n");
         break;
        }
      fprintf(packetInfoFile, "%9.4lf", systemClockTime);
      fprintf(packetInfoFile, " %8d", muxRate);
      packetsInPack = 0;

      errorCode = ReadStartCode(inputStream, &startCode);
      if (errorCode != 0) {
         fprintf(stderr, "Error in reading start code after pack header\n");
         break;
        }
      if (startCode == SYSTEM_HEADER_START_CODE) {
#ifdef __LOGGING_ON__
         fprintf(errorOutput, "SYSTEM HEADER:\n");
#endif
         ++numSystemHeaders;
         errorCode = ReadSystemHeader(inputStream);
         if (errorCode != 0) {
            fprintf(stderr, "Error in reading system header\n");
            break;
           }
         errorCode = ReadStartCode(inputStream, &startCode);
         if (errorCode != 0) {
           fprintf(stderr,"Error in reading start code after system header\n");
           break;
          }
        }
      while ((startCode & PACKET_START_CODE_MASK) == PACKET_START_CODE_PREFIX) {
         ++numPackets;
         ++packetsInPack;
         packetID = startCode & 0xff;
#ifdef __LOGGING_ON__
         fprintf(errorOutput, "PACKET ID %02x:\n", packetID);
#endif
         if (packetsInPack > 1) {
            fprintf(packetInfoFile, "%9s", "");
            fprintf(packetInfoFile, " %8s", "");
           }
         fprintf(packetInfoFile, " %7d", numPackets);
         errorCode = ReadPacket(packetID, inputStream, videoOutputStream,
                                audioOutputStream, packetInfoFile);
         fprintf(packetInfoFile, " \n");
         if (errorCode != 0) {
            fprintf(stderr,"Error in reading packet\n");
            break;
           }
         errorCode = ReadStartCode(inputStream, &startCode);
         if (errorCode != 0) {
            fprintf(stderr,"Error in start code after packet\n");
            break;
           }
         if (startCode == PACK_START_CODE || startCode == ISO_11172_END_CODE)
            break;
        }
      if (startCode == ISO_11172_END_CODE) {
         fprintf(stderr, "Successful parse of MPEG system level\n");
         break;
        }
      if (errorCode != 0)
         break;
     }
   fprintf(stderr, "%d system headers, %d packs, %d packets\n",
           numSystemHeaders, numPacks, numPackets);
   fprintf(stderr, "%d audio packets, %d video packets, %d padding packets\n",
           gNumAudioPackets, gNumVideoPackets, gNumPaddingPackets);
   fprintf(stderr, "%d reserved packets, %d/%d private type 1/2 packets\n",
           gNumReservedPackets, gNumPrivate_1_Packets, gNumPrivate_2_Packets);
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
int ReadStartCode(
   FILE *inputStream, 
   unsigned int *startCode)
  {
   int numRead;

   numRead = fread((unsigned char *)startCode, 1, 4, inputStream);
   *startCode=htonl(*startCode);

   if (numRead < 4) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "Error in reading start code, only got %d bytes\n", 
              numRead);
#endif
      return 1;
     }
#ifdef __LOGGING_ON__
   fprintf(errorOutput, "Read start code: %08x\n", *startCode);
#endif
   return 0;
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
int ReadPackHeader(
   FILE *inputStream,
   double *systemClockTime,
   unsigned long *muxRate)
  {
   int numRead;
   unsigned char inputBuffer[PACK_HEADER_SIZE];
   unsigned long systemClockRef;
   unsigned char systemClockRefHiBit;
   int errorCode;
   
   numRead = fread(inputBuffer, 1, PACK_HEADER_SIZE, inputStream);
   if (numRead < PACK_HEADER_SIZE) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, 
              "Error in reading Pack header, only got %d bytes\n", numRead);
#endif
      return 1;
     }
   ReadTimeStamp(inputBuffer, &systemClockRefHiBit, &systemClockRef);
   errorCode = MakeFloatClockTime(systemClockRefHiBit, systemClockRef, 
                                  systemClockTime);
   ReadRate(&inputBuffer[5], muxRate);
   *muxRate *= MUX_RATE_SCALE_FACTOR;
#ifdef __LOGGING_ON__
   fprintf(errorOutput, "System clock reference: %d, %lu (0x%x%08x)\n",
           (int)systemClockRefHiBit, systemClockRef,
           (int)systemClockRefHiBit, systemClockRef);
   if (errorCode == 0)
      fprintf(errorOutput, "System clock time: %1.4lf\n", *systemClockTime);
   else
      fprintf(errorOutput, "Error reading system clock time\n");
   fprintf(errorOutput, "muxRate: %lu (0x%08x)\n", *muxRate, *muxRate);
#endif
   return 0;
  }

/*
 *------------------------------------------------------------------
 *
 *   ReadSystemHeader
 *
 *      Parse out the system header, setup out dtream IDs for parsing packets
 *
 *   Results:  Returns 1 on error, 0 on success.
 *             Sets gAudioStreamID and gVideoStreamID
 *
 *------------------------------------------------------------------
 */
int ReadSystemHeader(
   FILE *inputStream)
  { 
   unsigned char *inputBuffer = NULL;
   int numRead;
   int pos,i;
   unsigned short headerSize;
   unsigned char streamID;
#ifdef __LOGGING_ON__
   unsigned long rateBound;
   unsigned long audioBound;
   unsigned char fixedFlag;
   unsigned char cspsFlag;
   unsigned long videoBound;
   unsigned char sysAudioLockFlag;
   unsigned char sysVideoLockFlag;
   unsigned char stdBufferScale;
   unsigned long stdBufferSize;
#endif

   numRead = fread((char *)&headerSize, 1, 2, inputStream); 
   headerSize=ntohs(headerSize);
   fprintf(errorOutput,"Header Size is %d (at %d)\n",headerSize,(int)ftell(inputStream));
   if (numRead != 2) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, 
              "Error in reading System header size, only got %d bytes\n", 
              numRead);
#endif
      return 1;
     }
   inputBuffer = malloc(headerSize);
   if (inputBuffer == NULL) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, 
              "Error in allocating %d bytes\n", headerSize);
#endif
      return 1;
    }
   inputBuffer[headerSize]=0;
   numRead = fread(inputBuffer, 1, headerSize, inputStream); 
#ifdef __LOGGING_ON__
/*
   for(i=0;i<headerSize;i++) 
      fprintf(errorOutput, 
              "%x ",*(inputBuffer+i));
   fprintf(errorOutput,"\n");
*/
#endif
   if (numRead < headerSize) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, 
              "Error in reading System Header, only got %d bytes\n", 
              numRead);
#endif
      return 1;
     }
#ifdef __LOGGING_ON__
   ReadRate(&inputBuffer[0], &rateBound);
   rateBound *= MUX_RATE_SCALE_FACTOR;
   fprintf(errorOutput, "rate_bound: %lu (0x%08x)\n", rateBound, rateBound);
   audioBound = (unsigned long)inputBuffer[3] >> 2;
   fprintf(errorOutput, "audio_bound: %lu (0x%08x)\n", audioBound, audioBound);
   fixedFlag = (inputBuffer[3] >> 1) & 0x01;
   fprintf(errorOutput, "fixed_flag: %d\n", fixedFlag);
   cspsFlag = inputBuffer[3] & 0x01;
   fprintf(errorOutput, "CSPS_flag: %d\n", cspsFlag);
   videoBound = (unsigned long)inputBuffer[4] & 0x1f;
   fprintf(errorOutput, "video_bound: %lu (0x%08x)\n", videoBound, videoBound);
   sysAudioLockFlag = (inputBuffer[4] & 0x80) >> 7;
   fprintf(errorOutput, "system_audio_lock_flag: %d\n", sysAudioLockFlag);
   sysVideoLockFlag = (inputBuffer[4] & 0x40) >> 6;
   fprintf(errorOutput, "system_video_lock_flag: %d\n", sysVideoLockFlag);
#endif

   pos = 6;
   while ((inputBuffer[pos] & 0x80) == 0x80) {
      streamID = inputBuffer[pos];
      gStreamID[gNumStreams++] = streamID;
#ifdef __LOGGING_ON__
      ReadSTD(&inputBuffer[pos + 1], &stdBufferScale, &stdBufferSize);
      fprintf(errorOutput, 
              "Read STD_buffer_scale = %d, STD_buffer_size = %lu (0x%0x)\n",
              (int)stdBufferScale, stdBufferSize, stdBufferSize);
      
      fprintf(errorOutput, "System Header: stream with ID 0x%x\n", streamID); 
#endif
      switch (streamID) {
         case STD_VIDEO_STREAM_ID: 
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "System Header: Std video stream\n"); 
#endif
            break;
         case STD_AUDIO_STREAM_ID: 
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "System Header: Std audio stream\n"); 
#endif
            break;
         case RESERVED_STREAM_ID: 
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "System Header: Reserved stream\n"); 
#endif
            break;
         case PADDING_STREAM_ID: 
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "System Header: Padding stream\n"); 
#endif
            break;
         case PRIVATE_STREAM_1_ID: 
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "System Header: Private (1) stream\n"); 
#endif
            break;
         case PRIVATE_STREAM_2_ID: 
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "System Header: Private (2) stream\n"); 
#endif
            break;
         default:
            if (streamID < MIN_STREAM_ID_ID) {
#ifdef __LOGGING_ON__
               fprintf(errorOutput, "System Header: Illegal stream ID (%x at %d)\n",streamID,(int)ftell(inputStream)); 
#endif
              return 1; 
              }
            switch (streamID >> 4) {
               case 0xc:
               case 0xd:
#ifdef __LOGGING_ON__
                  fprintf(errorOutput, "System Header: audio stream #%d\n",
                          (streamID & 0x1f)); 
#endif
                  if ((gAudioStreamID != 0) && (gAudioStreamID!=streamID)){
       fprintf(stderr, "This program can only handle a single audio stream\n");
       break;
                    }
                  gAudioStreamID = streamID;
                  break;
               case 0xe:
#ifdef __LOGGING_ON__
                  fprintf(errorOutput, "System Header: video stream #%d\n",
                          (streamID & 0xf)); 
#endif
                  if ((gVideoStreamID != 0) && (gVideoStreamID != streamID)) {
       fprintf(stderr, "This program can only handle a single video stream\n");
       break;
                    }
                  gVideoStreamID = streamID;
                  break;
               case 0xf:
                  gReservedStreamID = streamID;
#ifdef __LOGGING_ON__
                  fprintf(errorOutput, "System Header: reserved stream #%d\n",
                          (streamID & 0xf)); 
#endif
                  break;
              }
            break;
        }
      if (gNumStreams >= MAX_STREAMS) {
#ifdef __LOGGING_ON__
         fprintf(errorOutput, 
                 "This program can only handle %d streams in system header\n", 
                 MAX_STREAMS);
#endif
         return 1;
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
 *      Reads a single packet out of the stream, and puts it in
 *      the appropriate file.
 *
 *  Results:
 *      Parses the stream some, write to the others
 *
 *-----------------------------------------------------------------
 */   
int ReadPacket(
   unsigned char packetID,
   FILE *inputStream,
   FILE *videoOutputStream,
   FILE *audioOutputStream,
   FILE *packetInfoFile)
  {   
   int ioBytes;
   char inputBuffer[6];
   unsigned short packetLength;
   unsigned char *packetBuffer = NULL;
   int pos;
   unsigned char nextByte;
   int numStuffBytes = 0;
   int packetDataLength;
   unsigned char stdBufferScale;
   unsigned long stdBufferSize;
   unsigned char presTimeStampHiBit;
   unsigned long presTimeStamp;
   double presStampClockTime = 0.0;
   unsigned char decodeTimeStampHiBit;
   unsigned long decodeTimeStamp;
   double decodeStampClockTime = 0.0;
   char packetLengthString[20];
   int bufferSize = 0;
#ifdef __LOGGING_ON__
   int errorCode;
#endif

   ioBytes = fread(&packetLength, 1, 2, inputStream);
   packetLength=htons(packetLength);
   if (ioBytes < 2) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "ReadPacket: Error in reading packet length\n");
#endif
      return 1;
     }
#ifdef __LOGGING_ON__
   fprintf(errorOutput, 
           "input packet with ID %02x has length = %d at file offset %d\n", 
           packetID, packetLength, (int)ftell(inputStream));
#endif
   if (packetID == gAudioStreamID) {
      fprintf(packetInfoFile, " %4s", "AUD");
      ++gNumAudioPackets;
     }
   else if (packetID == gVideoStreamID) {
      fprintf(packetInfoFile, " %4s", "VID");
      ++gNumVideoPackets;
     }
   else {
      switch (packetID) {
         case PADDING_STREAM_ID:
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "Padding packet.\n");
#endif
            fprintf(packetInfoFile, " %4s", "PAD");
            ++gNumPaddingPackets;
            break;
         case RESERVED_STREAM_ID:
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "Reserved packet.\n");
#endif
            fprintf(packetInfoFile, " %4s", "RES");
            ++gNumReservedPackets;
            break;
         case PRIVATE_STREAM_1_ID:
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "Private packet type 1.\n");
#endif
            fprintf(packetInfoFile, " %4s", "PVT1");
            ++gNumPrivate_1_Packets;
            break;
         case PRIVATE_STREAM_2_ID:
#ifdef __LOGGING_ON__
            fprintf(errorOutput, "Private packet type 2.\n");
#endif
            fprintf(packetInfoFile, " %4s", "PVT2");
            ++gNumPrivate_2_Packets;
            break;
         default:
            fprintf(packetInfoFile, " %4s", "???");
            fprintf(stderr, "Unknown packet type encountered.\n");
        }
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "Skipping over this packet.\n");
#endif
      fseek(inputStream, packetLength, 1);
      sprintf(packetLengthString, "(total) %d", packetLength);
      fprintf(packetInfoFile, " %15s", packetLengthString);
      return 0;
     }
   packetBuffer = malloc((int)packetLength);
   if (packetBuffer == NULL) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "ReadPacket: Error in allocating %d bytes\n",
              packetLength);
#endif
      return 1;
     }
   ioBytes = fread(packetBuffer, 1, packetLength, inputStream);
   if (ioBytes < packetLength) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, 
              "Error in reading packet data, only read %d bytes\n", ioBytes);
#endif
      return 1;
     }
   pos = 0;
   while (packetBuffer[pos] & 0x80) {
      ++numStuffBytes;
#ifdef __LOGGING_ON__
      if (packetBuffer[pos] != 0xff)
         fprintf(errorOutput, "Warning: stuffing byte = 0x%x not 0xff\n", 
                 (int)packetBuffer[pos]);
#endif
      ++pos;
     }
   if (numStuffBytes > 0)
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "Skipped %d stuffing bytes\n", numStuffBytes);
#endif
   nextByte = packetBuffer[pos];
   if ((nextByte >> 6) == 0x01) {
#ifdef __LOGGING_ON__
      ReadSTD(&packetBuffer[pos], &stdBufferScale, &stdBufferSize);
      fprintf(errorOutput, 
              "Read STD_buffer_scale = %d, STD_buffer_size = %lu (0x%0x)\n",
              (int)stdBufferScale, stdBufferSize, stdBufferSize);
      if (stdBufferScale == 0)
         bufferSize = 128*stdBufferSize;
      else if (stdBufferScale == 1)
         bufferSize = 1024*stdBufferSize;
#endif
      pos += 2;
      nextByte = packetBuffer[pos];
     } 
   if ((nextByte >> 4) == 0x02) {
#ifdef __LOGGING_ON__
      ReadTimeStamp(&packetBuffer[pos], &presTimeStampHiBit, &presTimeStamp);
      fprintf(errorOutput, "Read presentation_time_stamp %d, %lu (0x%x%0x)\n",
              (int)presTimeStampHiBit, presTimeStamp,
              (int)presTimeStampHiBit, presTimeStamp);
      errorCode = MakeFloatClockTime(presTimeStampHiBit, presTimeStamp, 
                                     &presStampClockTime);
      if (errorCode == 0)
         fprintf(errorOutput, "Presentation stamp clock time: %1.4lf\n", 
                 presStampClockTime);
      else
         fprintf(errorOutput, "Error reading presentation stamp clock time\n");
#endif
      pos += 5;
     } 
   else if ((nextByte >> 4) == 0x03) {
#ifdef __LOGGING_ON__
      ReadTimeStamp(&packetBuffer[pos], &presTimeStampHiBit, &presTimeStamp);
      fprintf(errorOutput, "Read presentation_time_stamp %d, %lu (0x%x%0x)\n",
              (int)presTimeStampHiBit, presTimeStamp,
              (int)presTimeStampHiBit, presTimeStamp);
      errorCode = MakeFloatClockTime(presTimeStampHiBit, presTimeStamp, 
                                     &presStampClockTime);
      if (errorCode == 0)
         fprintf(errorOutput, "Presentation stamp clock time: %1.4lf\n", 
                 presStampClockTime);
      else
         fprintf(errorOutput, "Error reading presentation stamp clock time\n");
      ReadTimeStamp(&packetBuffer[pos + 5], &decodeTimeStampHiBit, 
                    &decodeTimeStamp);
      fprintf(errorOutput, "Read decoding time stamp %d, %lu (0x%x%0x)\n",
              (int)decodeTimeStampHiBit, decodeTimeStamp,
              (int)decodeTimeStampHiBit, decodeTimeStamp);
      errorCode = MakeFloatClockTime(decodeTimeStampHiBit, decodeTimeStamp, 
                                     &decodeStampClockTime);
      if (errorCode == 0)
         fprintf(errorOutput, "Decoding stamp clock time: %1.4lf\n", 
                 decodeStampClockTime);
      else
         fprintf(errorOutput, "Error in reading decoding stamp clock time\n");
#endif
      pos += 10;
     } 
   else {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "Read 0x%02x (s.b. 0x0f)\n", nextByte);
#endif
      pos += 1;
     }
   packetDataLength = packetLength - pos;
   sprintf(packetLengthString, "%d(%d)+%d", 
           pos, numStuffBytes, packetDataLength);
   fprintf(packetInfoFile, " %15s", packetLengthString);
   if (bufferSize != 0)
      fprintf(packetInfoFile, " %10d", bufferSize);
   else
      fprintf(packetInfoFile, " %12s", "");
   if (presStampClockTime != 0.0)
      fprintf(packetInfoFile, " %9.4lf", presStampClockTime);
   else
      fprintf(packetInfoFile, " %9s", "");
   if (decodeStampClockTime != 0.0)
      fprintf(packetInfoFile, " %9.4lf", decodeStampClockTime);
   else
      fprintf(packetInfoFile, " %9s", "");
   if (packetID == gVideoStreamID) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "Writing Video packet of length %d\n",
              packetDataLength);
#endif
      ioBytes = fwrite(packetBuffer + pos, 1, packetDataLength, 
                       videoOutputStream);
      if (ioBytes != packetDataLength) {
#ifdef __LOGGING_ON__
         fprintf(errorOutput, "Error in writing video packet\n"); 
#endif
         return 1;
        }
     }
   else if (packetID == gAudioStreamID) {
#ifdef __LOGGING_ON__
      fprintf(errorOutput, "Writing Audio packet of length %d\n",
              packetDataLength);
#endif
      ioBytes = fwrite(packetBuffer + pos, 1, packetDataLength, 
                       audioOutputStream);
      if (ioBytes != packetDataLength) {
#ifdef __LOGGING_ON__
         fprintf(errorOutput, "Error in writing audio packet\n"); 
#endif
         return 1;
        }
     }
   free(packetBuffer);
   return 0; 
  }

void ReadTimeStamp(
   unsigned char *inputBuffer,
   unsigned char *hiBit,
   unsigned long *low4Bytes)
  {
   *hiBit = ((unsigned long)inputBuffer[0] >> 3) & 0x01;
   *low4Bytes = (((unsigned long)inputBuffer[0] >> 1) & 0x03) << 30; 
   *low4Bytes |= (unsigned long)inputBuffer[1] << 22; 
   *low4Bytes |= ((unsigned long)inputBuffer[2] >> 1) << 15; 
   *low4Bytes |= (unsigned long)inputBuffer[3] << 7; 
   *low4Bytes |= ((unsigned long)inputBuffer[4]) >> 1; 
  }

void ReadSTD(
   unsigned char *inputBuffer,
   unsigned char *stdBufferScale,
   unsigned long *stdBufferSize) 
  {
   *stdBufferScale = ((inputBuffer[0] & 0x20) >> 5); 
   *stdBufferSize = ((unsigned long)inputBuffer[0] & 0x1f) << 8;
   *stdBufferSize |= (unsigned long)inputBuffer[1];
  }

void ReadRate(
   unsigned char *inputBuffer,
   unsigned long *rate)
  {
   *rate = (inputBuffer[0] & 0x7f) << 15;
   *rate |= inputBuffer[1] << 7;
   *rate |= (inputBuffer[2] & 0xfe) >> 1;
  }

#define FLOAT_0x10000 (double)((unsigned long)1 << 16)

int MakeFloatClockTime(
   unsigned char hiBit,
   unsigned long low4Bytes,
   double *floatClockTime)
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


void PrintUsage(
   char *programName)
  {
   fprintf(stderr, "Usage: %s <input file name>\n", programName);
  }

void ErrorExit(
   char *errorMessage)
  {
   fprintf(stderr, "%s\n", errorMessage);
   exit(1);
  }
