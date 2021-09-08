#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <openssl/md5.h>

int styx_stat_filesize(const char*);

int styx_mv(const char*, const char*);

int styx_cp(const char*, const char*);

const char* styx_md5(const char*);
