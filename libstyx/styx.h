#ifndef STYX_H
#define STYX_H

#include <sys/types.h>

/*
 * libstyx — pluto's C companion (loaded by the styx lisp system)
 *
 * conventions:
 *   - functions returning char* return a malloc'd hex string that
 *     the CALLER must free (the lisp side uses cffi's
 *     :free-from-foreign), or NULL on error
 *   - functions returning integers return -1 on error
 */

off_t styx_stat_filesize(const char* afilename, int follow_symlinks);
int   styx_stat_is_symlink_p(const char* afilename);

/* a plain predicate, not the -1 convention: 1 if fd refers to
 * a terminal, 0 if not (a bad fd is simply not a terminal) */
int   styx_isatty(int fd);

/* one TIOCGWINSZ ioctl fills both out-params;
 * 0 on success, -1 if fd isn't a terminal */
int   styx_terminal_size(int fd, int* rows, int* cols);

char* styx_md5_data(const char* data, long len);
char* styx_md5_string(const char* astring);
char* styx_md5_file(const char* afilename);

char* styx_sha256_data(const char* data, long len);
char* styx_sha256_string(const char* astring);
char* styx_sha256_hexstring(const char* astring);
char* styx_sha256_file(const char* afilename);

char* styx_sha512_data(const char* data, long len);
char* styx_sha512_string(const char* astring);
char* styx_sha512_file(const char* afilename);

char* styx_ripemd160_data(const char* data, long len);
char* styx_ripemd160_string(const char* astring);
char* styx_ripemd160_hexstring(const char* astring);

#endif
