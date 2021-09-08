#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/sendfile.h>
#include <sys/stat.h>



int styx_stat_filesize(const char* afilename){
	struct stat st;
	stat(afilename, &st);
	return st.st_size;
}

int styx_mv(const char *old, const char *new){
	int ret;
	ret = rename(old, new);
	return ret;
}

int styx_cp(const char* source, const char* destination){
	int input, output;
	if ((input = open(source, O_RDONLY)) == -1){
		return -1;
	}
	if ((output = creat(destination, 0660)) == -1){
		close(input);
		return -1;
	}

	off_t bytesCopied = 0;
	struct stat fileinfo = {0};
	fstat(input, &fileinfo);
	int result = sendfile(output, input, &bytesCopied, fileinfo.st_size);

	close(input);
	close(output);
	return result;
}

