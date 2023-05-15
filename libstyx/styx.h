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

off_t styx_stat_filesize(const char*, int);
int styx_stat_is_symlink_p(const char*);

char* styx_hexstringtobytes(const char*);

char* styx_md5_data(const char*, long);
const char* styx_md5_string(const char*);
const char* styx_md5_file(const char*);

char* styx_sha256_data(const char*, long);
char* styx_sha256_string(const char*);
const char* styx_sha256_hexstring(const char*);
const char* styx_sha256_file(const char*);

char* styx_sha512_data(const char*, long);
const char* styx_sha512_string(const char*);
const char* styx_sha512_file(const char*);

char* styx_ripemd160_string(const char*);
const char* styx_ripemd160_hexstring(const char*);
