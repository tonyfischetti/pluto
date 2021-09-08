#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/sendfile.h>
#include <sys/stat.h>

int styx_stat_filesize(const char* afilename);

int styx_mv(const char*, const char*);

int styx_cp(const char*, const char*);
