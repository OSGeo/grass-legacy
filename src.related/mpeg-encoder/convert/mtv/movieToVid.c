#include <stdio.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <X11/Xlib.h>
#include <XPlxExt.h>
#include <audio_hdr.h>
#include <audio_filehdr.h>
#include <movie.h>

int fileSize;
char *fileData;
int fd;


/*
 *--------------------------------------------------------------
 *
 * MapFile --
 *
 *	Memory map the file passed in.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	This initializes the global variable fileData and fd
 *
 *--------------------------------------------------------------
 */
void
MapFile (filename)
    char *filename;
{
    struct stat status;

    fd = open (filename, O_RDONLY, 0);
    if (fd == -1) {
	perror (filename);
	exit (-1);
    }
    stat(filename, &status);
    fileSize = status.st_size;
    fileData = mmap(0, fileSize, PROT_READ, MAP_SHARED, fd, 0);
}


/*
 *--------------------------------------------------------------
 *
 * WriteScriptFile --
 *
 *	Write the offset file (filename.ofs) 
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Program may exit on various errors.
 *
 *--------------------------------------------------------------
 */

WriteScriptFile (name, dataDir, indexDir, host, headerPtr, offsets, numFrames)
    char *name, *dataDir;
    char *indexDir, *host;
    jpheader *headerPtr;
    int *offsets;
    int numFrames;
{
    FILE *outFile;
    char str[512];
    int i, max, size;

    sprintf (str, "%s/%s.script", indexDir, name);
    outFile = fopen (str, "w");
    if (outFile == NULL) {
	perror (str);
	exit (1);
    }

    max = -1;
    for (i=0; i<numFrames; i++) {
	size = offsets[i+1] - offsets[i];
	if (size > max) max = size;
    }

    fprintf (outFile, "#%%!CM-Script-1.0\n\n");
    fprintf (outFile, "set path \"%s/%s\"\n", dataDir, name);
    fprintf (outFile, "set videoClipList \"%s {$path.vid $path.ofs 0 end}\"\n",
	     host);
    fprintf (outFile, "set audioClipList \"%s {$path.au 0 end}\"\n", host);
    fprintf (outFile, "set width   %d\n", headerPtr->width);
    fprintf (outFile, "set height  %d\n", headerPtr->height);
    fprintf (outFile, "set maxSize %d\n", max+100);
    fprintf (outFile, "set time    %d\n", 1+(numFrames/headerPtr->fps));
    fprintf (outFile, "set qfactor %d\n", headerPtr->qfactor);

    fclose (outFile);
}



/*
 *--------------------------------------------------------------
 *
 * WriteOfsFile --
 *
 *	Write the offset file (filename.ofs) 
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Program may exit on various errors.
 *
 *--------------------------------------------------------------
 */

WriteOfsFile (name, dir, headerPtr, offsets, numFrames)
    char *name, *dir;
    jpheader *headerPtr;
    int *offsets;
    int numFrames;
{
    FILE *outFile;
    char str[512];
    int i, max, size;

    sprintf (str, "%s/%s.ofs", dir, name);
    outFile = fopen (str, "w");
    if (outFile == NULL) {
	perror (str);
	exit (1);
    }

    max = -1;
    for (i=0; i<numFrames; i++) {
	size = offsets[i+1] - offsets[i];
	if (size > max) max = size;
    }

    fprintf (outFile, "%s.vid\n", name);
    fprintf (outFile, "%d %d %d %d %d %d\n", numFrames,
		headerPtr->width, headerPtr->height,
		max, headerPtr->qfactor, headerPtr->fps);

    for (i=0; i<numFrames; i++) {
	fprintf (outFile, "%d\n", offsets[i]);
    }
    fclose (outFile);
}


/*
 *--------------------------------------------------------------
 *
 * WriteVidFile --
 *
 *	Write the video data file (filename.vid) This is a little
 *	state machine that interprets the data in the file.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Program may exit on various errors.
 *
 *--------------------------------------------------------------
 */

WriteVidFile (name, dir, headerPtr, fot, numFrames, offset)
    char *name, *dir;
    jpheader *headerPtr;
    int *fot;
    int numFrames;
    int *offset;
{
    FILE *vidFile;
    FILE *audFile;
    char vidStr[512];
    char audStr[512];
    int i, size, pos;
    unsigned char cmd;
    int found, skipping;
    int count, numOut ;
    XPlxCImage image;
    Audio_filehdr audHeader;
    int percent;

    image.width = headerPtr->width;
    image.height = headerPtr->height;
    image.info = NULL;
    sprintf (vidStr, "%s/%s.vid", dir, name);
    vidFile = fopen (vidStr, "w");
    if (vidFile == NULL) {
	perror (vidStr);
	exit (1);
    }

    sprintf (audStr, "%s/%s.au", dir, name);
    audFile = fopen (audStr, "w");
    if (audFile == NULL) {
	perror (audStr);
	exit (1);
    }
    audHeader.magic = AUDIO_FILE_MAGIC;
    audHeader.hdr_size = sizeof(Audio_filehdr);
    audHeader.data_size = AUDIO_UNKNOWN_SIZE;
    audHeader.encoding = headerPtr->audio.encoding;
    audHeader.sample_rate = headerPtr->audio.sample_rate;
    audHeader.channels = headerPtr->audio.channels;

    fwrite (&audHeader, 1, sizeof(Audio_filehdr), audFile);
    count = 0;
    fprintf (stderr, "Writing audio and video files... (0%%)");
    percent = 0;
    for (i=0; i<numFrames; i++) {
	if (percent != 100*i/numFrames) {
	    percent = 100*i/numFrames;
	    fprintf (stderr, "\rWriting audio and video files... (%d%%)",
		     percent);
	}
	pos = fot[i];
	skipping = 0;
	found = 0;
	while (!found) {
	    cmd = fileData[pos];
	    switch (cmd) {
		case LOAD_AUDIO_0:
		    pos++;
		    fwrite (&fileData[pos], 1, headerPtr->audioslice, audFile);
		    pos += headerPtr->audioslice;
		    break;

		case LOAD_AUDIO_1:
		case LOAD_AUDIO_2:
		case LOAD_AUDIO_3:
		    pos++;
		    pos += headerPtr->audioslice;
		    break;

		case LOAD_JPEG:
		    pos++;
		    bcopy (&fileData[pos], &size, sizeof(int));
		    pos += sizeof(int);
		    found = 1;
		    break;

		case END_FRAME:
		    pos++;
		    found = 1;
		    break;

		default:
		    if (!skipping) {
			fprintf (stderr, "Unrecognized command char %x\n", cmd);
			fprintf (stderr, "Skipping...\n");
			skipping = 1;
		    }
		    pos++;
		    break;
	    }
	}
	offset[i] = count;
	count += sizeof(XPlxCImage) + size;

	image.size = size;
	image.data = fileData+pos;
	numOut = fwrite (&image, 1, sizeof(XPlxCImage), vidFile);
	if (numOut != sizeof(XPlxCImage)) {
	    perror (vidStr);
	    exit (1);
	}
	numOut = fwrite (image.data,  1, image.size, vidFile);
	if (numOut != image.size) {
	    perror (vidStr);
	    exit (1);
	}
    }
    fprintf (stderr, "done\n");
    offset[i] = count;
    fclose (vidFile);
    fclose (audFile);
}

void
usage (p)
    char *p;
{
    fprintf (stderr, "Usage: %s movieFile dataDir indexDir srcHostName\n", p);
    exit (0);
}

void
main (argc, argv)
int argc;
char **argv;
{
    jpheader header;
    int *offsets;
    int *fot;
    int numFrames;

    if (argc != 5) {
	usage (argv[0]);
    }

    MapFile (argv[1]);
    bcopy (fileData, &header, sizeof(jpheader));
    numFrames = header.frames;
    offsets = (int *)malloc((numFrames+1)*sizeof(int));
    fot = (int *)malloc(numFrames*sizeof(int));
    bcopy (fileData+header.indexbuf, fot, numFrames*sizeof(int));

    WriteVidFile (argv[1], argv[2], &header, fot, numFrames, offsets);

    fprintf (stderr, "Writing offset file...");
    WriteOfsFile (argv[1], argv[2], &header, offsets, numFrames);
    fprintf (stderr, "done\n");

    fprintf (stderr, "Writing script file...");
    WriteScriptFile (argv[1], argv[2], argv[3], argv[4],
		     &header, offsets, numFrames);
    fprintf (stderr, "done\n");
}
