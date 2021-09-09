#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <openssl/md5.h>



off_t styx_stat_filesize(const char* afilename){
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

const char* styx_md5(const char* afilename){
	FILE *fh;
	long filesize;
	unsigned char *buf;
	unsigned char md5_result[MD5_DIGEST_LENGTH];
	int i;
	unsigned char *ret_string = NULL;

	fh = fopen(afilename, "r");
	fseek(fh, 0L, SEEK_END);
	filesize = ftell(fh);
	fseek(fh, 0L, SEEK_SET);
	buf = malloc(filesize);
	fread(buf, filesize, 1, fh);
	fclose(fh);
	ret_string = malloc(MD5_DIGEST_LENGTH*2);
	MD5(buf, filesize, md5_result);
	for (i=0; i < MD5_DIGEST_LENGTH; i++){
		sprintf(&ret_string[i*2], "%02x", md5_result[i]);
	}
	free(buf);
	return ret_string;
}

