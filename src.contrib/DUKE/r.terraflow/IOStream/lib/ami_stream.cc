/*C
 * Original project: Lars Arge, Jeff Chase, Pat Halpin, Laura Toma, Dean
 *                   Urban, Jeff Vitter, Rajiv Wickremesinghe 1999
 * 
 * GRASS Implementation: Lars Arge, Helena Mitasova, Laura Toma 2002
 *
 * Copyright (c) 2002 Duke University -- Laura Toma 
 *
 * Copyright (c) 1999-2001 Duke University --
 * Laura Toma and Rajiv Wickremesinghe
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Duke University
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE TRUSTEES AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE TRUSTEES OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *C*/



#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include <ami_stream.h>

/**********************************************************************/
/* creates a random file name, opens the file for reading and writing
   and and returns a file descriptor */
int
ami_single_temp_name(char *base, char* tmp_path) {
 
  char *base_dir;
  int fd;

  // get the dir
  base_dir = getenv(STREAM_TMPDIR);
  assert(base_dir);

  sprintf(tmp_path, "%s/%s_XXXXXX", base_dir, base);
  fd  = mkstemp(tmp_path);

  if (fd == -1) {
    cerr <<  "ami_single_temp_name: ";
    perror("mkstemp failed: ");
    assert(0);
    exit(1);
  }
  return fd;
}


/**********************************************************************/
/* given fd=fide descriptor, associates with it a stream aopened in
   access_mode and returns it */
FILE* 
open_stream(int fd, AMI_stream_type st) {
  FILE* fp = NULL;
  
  assert(fd > -1);   
  switch (st) {
  case   AMI_READ_STREAM:
    fp = fdopen(fd, "rb");
    break;
  case   AMI_WRITE_STREAM:
    fp = fdopen(fd, "wb");
    break;
  case AMI_APPEND_STREAM:
    fp = fdopen(fd, "ab+");
    break;
  case AMI_READ_WRITE_STREAM: 
      fp = fdopen(fd, "rb+");
      if (!fp) {
	//if file does not exist, create it
	fp = fdopen(fd, "wb+");
      }
      break;
  }
  assert(fp);

  return fp;
}


/**********************************************************************/
/* open the file whose name is pathname in access mode */
FILE* 
open_stream(char* pathname, AMI_stream_type st) {

  FILE* fp = NULL;
  assert(pathname);

  switch (st) {
  case   AMI_READ_STREAM:
    fp = fopen(pathname, "rb");
    break;
  case   AMI_WRITE_STREAM:
    fp = fopen(pathname, "wb");
    break;
  case AMI_APPEND_STREAM:
    fp = fopen(pathname, "ab+");
    assert(fp);
    if (fseek (fp, 0, SEEK_END) == -1) {
      perror("AMI_STREAM: fseek failed ");
    }
    break;
  case AMI_READ_WRITE_STREAM: 
      fp = fopen(pathname, "rb+");
      if (!fp) {
	//if file does not exist, create it
      fp = fopen(pathname, "wb+");
      }
      break;
  }
  if (!fp) {
    perror("cannot open stream");
    assert(0);
    exit(1);
  }
  assert(fp);
  return fp;
}

