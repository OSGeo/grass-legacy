#include <string.h>
#include "driverlib.h"

static char filename[255];
static char charset[50] = "EUC-JP";

int init_font_freetype(char* name) {
	int len = 0;
	len = strlen(name);
	if(len>253) {
		strncpy(filename,name,len);
		filename[len+1] = '\0';
	}else{
		strncpy(filename,name,253);
		filename[254] = '\0';
	}
	return 0;
}

int init_font_charset(char* str) {
	int len = 0;
	len = strlen(str);
	if(len>47) {
		strncpy(charset,str,len);
		charset[len+1] = '\0';
	}else{
		strncpy(charset,str,48);
		charset[49] = '\0';
	}
	return 0;
}

char* getFreeTypeName() {
	return filename;
}

char* getCharset() {
	return charset;
}
