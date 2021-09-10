#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <openssl/md5.h>

off_t styx_stat_filesize(const char*, int);

int styx_stat_is_symlink_p(const char*);

// int styx_mv(const char*, const char*);
//
// int styx_cp(const char*, const char*);

const char* styx_md5(const char*);
