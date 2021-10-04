#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <openssl/md5.h>
#include <openssl/sha.h>
#include <openssl/ripemd.h>


off_t styx_stat_filesize(const char* afilename, int follow_symlinks){
	int ret;
	struct stat st;
	if(follow_symlinks){
		ret = stat(afilename, &st);
	}
	else{
		ret = lstat(afilename, &st);
	}
	if(ret!=0){
		return -1;
	}
	return st.st_size;
}

int styx_stat_is_symlink_p(const char* afilename){
	int ret;
	struct stat st;
	ret = lstat(afilename, &st);
	if(ret!=0){
		return -1;
	}
	return S_ISLNK(st.st_mode);
}


// stolen from stack overflow
/* unsigned char* hexstr_to_char(const char* hexstr){ */
unsigned char* styx_hexstringtobytes(const char* hexstr){
	size_t len = strlen(hexstr);
	size_t final_len = len / 2;
	unsigned char* chrs = (unsigned char*)malloc((final_len+1) * sizeof(*chrs));
	for (size_t i=0, j=0; j<final_len; i+=2, j++)
		chrs[j] = (hexstr[i] % 32 + 9) % 25 * 16 + (hexstr[i+1] % 32 + 9) % 25;
	chrs[final_len] = '\0';
	return chrs;
}


char* styx_md5_data(const char* data, long len){
	unsigned char md5_result[MD5_DIGEST_LENGTH];
	int i;
	unsigned char *ret_string = NULL;

	ret_string = malloc(MD5_DIGEST_LENGTH*2+1);

	MD5(data, len, md5_result);

	for (i=0; i < MD5_DIGEST_LENGTH; i++){
		sprintf(&ret_string[i*2], "%02x", md5_result[i]);
	}

	return ret_string;
}

char* styx_sha256_data(const char* data, long len){
	unsigned char sha256_result[SHA256_DIGEST_LENGTH];
	int i;
	unsigned char *ret_string = NULL;

	ret_string = malloc(SHA256_DIGEST_LENGTH*2+1);

	SHA256(data, len, sha256_result);

	for(int i = 0; i < SHA256_DIGEST_LENGTH; i++){
		sprintf(&ret_string[i*2], "%02x",
			(unsigned int)sha256_result[i]);
	}

	return ret_string;
}

char* styx_sha512_data(const char* data, long len){
	unsigned char sha512_result[SHA512_DIGEST_LENGTH];
	int i;
	unsigned char *ret_string = NULL;

	ret_string = malloc(SHA512_DIGEST_LENGTH*2+1);

	SHA512(data, len, sha512_result);

	for(int i = 0; i < SHA512_DIGEST_LENGTH; i++){
		sprintf(&ret_string[i*2], "%02x",
			(unsigned int)sha512_result[i]);
	}

	return ret_string;
}

const char* styx_md5_string(const char* astring){
	unsigned char *ret_string = NULL;
	int size = strlen(astring);
	ret_string = styx_md5_data(astring, size);
	return ret_string;
}

char* styx_sha256_string(const char* astring){
	unsigned char *ret_string = NULL;
	int size = strlen(astring);
	ret_string = styx_sha256_data(astring, size);
	return ret_string;
}

const char* styx_sha256_hexstring(const char* astring){
	unsigned char *ret_string = NULL;
	int size = strlen(astring);
	ret_string = styx_sha256_string(styx_hexstringtobytes(astring));
	return ret_string;
}

const char* styx_sha512_string(const char* astring){
	unsigned char *ret_string = NULL;
	int size = strlen(astring);
	ret_string = styx_sha512_data(astring, size);
	return ret_string;
}

const char* styx_md5_file(const char* afilename){
	FILE *fh;
	long filesize;
	unsigned char *buf;
	const unsigned char *ret_string = NULL;

	fh = fopen(afilename, "r");
	fseek(fh, 0L, SEEK_END);
	filesize = ftell(fh);
	fseek(fh, 0L, SEEK_SET);
	buf = malloc(filesize);
	fread(buf, filesize, 1, fh);
	fclose(fh);
	ret_string = styx_md5_data(buf, filesize);
	free(buf);
	return ret_string;
}

const char* styx_sha256_file(const char* afilename){
	FILE *fh;
	long filesize;
	unsigned char *buf;
	const unsigned char *ret_string = NULL;

	fh = fopen(afilename, "r");
	fseek(fh, 0L, SEEK_END);
	filesize = ftell(fh);
	fseek(fh, 0L, SEEK_SET);
	buf = malloc(filesize);
	fread(buf, filesize, 1, fh);
	fclose(fh);
	ret_string = styx_sha256_data(buf, filesize);
	free(buf);
	return ret_string;
}

const char* styx_sha512_file(const char* afilename){
	FILE *fh;
	long filesize;
	unsigned char *buf;
	const unsigned char *ret_string = NULL;

	fh = fopen(afilename, "r");
	fseek(fh, 0L, SEEK_END);
	filesize = ftell(fh);
	fseek(fh, 0L, SEEK_SET);
	buf = malloc(filesize);
	fread(buf, filesize, 1, fh);
	fclose(fh);
	ret_string = styx_sha512_data(buf, filesize);
	free(buf);
	return ret_string;
}

char* styx_ripemd160_data(const char* data, long len){
	unsigned char ripe_result[RIPEMD160_DIGEST_LENGTH];
	int i;
	unsigned char *ret_string = NULL;

	ret_string = malloc(RIPEMD160_DIGEST_LENGTH*2+1);

	RIPEMD160(data, len, ripe_result);

	for (i=0; i < RIPEMD160_DIGEST_LENGTH; i++){
		sprintf(&ret_string[i*2], "%02x", ripe_result[i]);
	}

	return ret_string;
}

char* styx_ripemd160_string(const char* astring){
	unsigned char *ret_string = NULL;
	int size = strlen(astring);
	ret_string = styx_ripemd160_data(astring, size);
	return ret_string;
}

const char* styx_ripemd160_hexstring(const char* astring){
	unsigned char *ret_string = NULL;
	int size = strlen(astring);
	ret_string = styx_ripemd160_string(styx_hexstringtobytes(astring));
	return ret_string;
}


