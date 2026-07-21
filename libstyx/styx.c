/*
 * styx.c — the C side of styx, pluto's syscall tier.
 *
 * Started while reading The Linux Programming Interface, as the
 * place where pluto reaches below what portable lisp can see:
 * stat, hashing (openssl), and friends. Anything that needs a
 * real syscall or a real C library gets a small, boring wrapper
 * here and a lisp face in styx.lisp.
 *
 * The Makefile in this directory builds libstyx.{dylib,so} and
 * `make install` copies it to ~/.lisp, where styx.lisp loads it
 * by absolute path. Return/ownership conventions are documented
 * in styx.h.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <openssl/evp.h>
#include <openssl/provider.h>

#include "styx.h"


/* ------------------------------------------------------- */
/* stat ------------------------------------------------- - */

off_t styx_stat_filesize(const char* afilename, int follow_symlinks){
	int ret;
	struct stat st;
	if(follow_symlinks){
		ret = stat(afilename, &st);
	}
	else{
		ret = lstat(afilename, &st);
	}
	if(ret != 0){
		return -1;
	}
	return st.st_size;
}

int styx_stat_is_symlink_p(const char* afilename){
	struct stat st;
	if(lstat(afilename, &st) != 0){
		return -1;
	}
	return S_ISLNK(st.st_mode);
}


/* ------------------------------------------------------- */
/* terminal things --------------------------------------- */

int styx_isatty(int fd){
	return isatty(fd);
}


/* ------------------------------------------------------- */
/* hashing (openssl 3 EVP api) --------------------------- */

static char* to_hex(const unsigned char* digest, unsigned int len){
	char* out = malloc((size_t)len * 2 + 1);
	if(!out){
		return NULL;
	}
	for(unsigned int i = 0; i < len; i++){
		sprintf(out + i * 2, "%02x", digest[i]);
	}
	return out;
}

/* RIPEMD160 (et al.) live in the "legacy" provider in openssl 3,
 * which isn't loaded by default; retry with it before giving up */
static EVP_MD* fetch_md(const char* name){
	EVP_MD* md = EVP_MD_fetch(NULL, name, NULL);
	if(!md){
		static int legacy_tried = 0;
		if(!legacy_tried){
			legacy_tried = 1;
			OSSL_PROVIDER_load(NULL, "legacy");
			OSSL_PROVIDER_load(NULL, "default");
		}
		md = EVP_MD_fetch(NULL, name, NULL);
	}
	return md;
}

static char* hash_data(const char* mdname, const void* data, size_t len){
	char* result = NULL;
	unsigned char digest[EVP_MAX_MD_SIZE];
	unsigned int dlen = 0;
	EVP_MD* md = fetch_md(mdname);
	if(!md){
		return NULL;
	}
	if(EVP_Digest(data, len, digest, &dlen, md, NULL)){
		result = to_hex(digest, dlen);
	}
	EVP_MD_free(md);
	return result;
}

/* streams the file through the digest in 64KB chunks
 * (never slurps the whole file into memory) */
static char* hash_file(const char* mdname, const char* afilename){
	char* result = NULL;
	FILE* fh = fopen(afilename, "rb");
	if(!fh){
		return NULL;
	}
	EVP_MD* md = fetch_md(mdname);
	if(!md){
		fclose(fh);
		return NULL;
	}
	EVP_MD_CTX* ctx = EVP_MD_CTX_new();
	if(ctx && EVP_DigestInit_ex(ctx, md, NULL)){
		unsigned char buf[1 << 16];
		size_t n;
		int ok = 1;
		while((n = fread(buf, 1, sizeof buf, fh)) > 0){
			if(!EVP_DigestUpdate(ctx, buf, n)){
				ok = 0;
				break;
			}
		}
		if(ok && !ferror(fh)){
			unsigned char digest[EVP_MAX_MD_SIZE];
			unsigned int dlen = 0;
			if(EVP_DigestFinal_ex(ctx, digest, &dlen)){
				result = to_hex(digest, dlen);
			}
		}
	}
	EVP_MD_CTX_free(ctx);
	EVP_MD_free(md);
	fclose(fh);
	return result;
}

static int hexval(int c){
	if(c >= '0' && c <= '9'){
		return c - '0';
	}
	c = tolower(c);
	if(c >= 'a' && c <= 'f'){
		return c - 'a' + 10;
	}
	return -1;
}

/* validates and converts; the byte count goes in *out_len
 * (embedded NULs are fine — this is why the hexstring hashers
 * must NOT go through strlen) */
static unsigned char* hexstring_to_bytes(const char* hexstr, size_t* out_len){
	size_t len = strlen(hexstr);
	if(len % 2 != 0){
		return NULL;
	}
	size_t final_len = len / 2;
	unsigned char* bytes = malloc(final_len ? final_len : 1);
	if(!bytes){
		return NULL;
	}
	for(size_t i = 0, j = 0; j < final_len; i += 2, j++){
		int hi = hexval((unsigned char)hexstr[i]);
		int lo = hexval((unsigned char)hexstr[i + 1]);
		if(hi < 0 || lo < 0){
			free(bytes);
			return NULL;
		}
		bytes[j] = (unsigned char)(hi * 16 + lo);
	}
	*out_len = final_len;
	return bytes;
}

static char* hash_hexstring(const char* mdname, const char* hexstr){
	size_t len = 0;
	unsigned char* bytes = hexstring_to_bytes(hexstr, &len);
	if(!bytes){
		return NULL;
	}
	char* result = hash_data(mdname, bytes, len);
	free(bytes);
	return result;
}


/* ------------------------------------------------------- */
/* the exported hashers ---------------------------------- */

char* styx_md5_data(const char* data, long len){
	return hash_data("MD5", data, (size_t)len);
}
char* styx_md5_string(const char* astring){
	return hash_data("MD5", astring, strlen(astring));
}
char* styx_md5_file(const char* afilename){
	return hash_file("MD5", afilename);
}

char* styx_sha256_data(const char* data, long len){
	return hash_data("SHA256", data, (size_t)len);
}
char* styx_sha256_string(const char* astring){
	return hash_data("SHA256", astring, strlen(astring));
}
char* styx_sha256_hexstring(const char* astring){
	return hash_hexstring("SHA256", astring);
}
char* styx_sha256_file(const char* afilename){
	return hash_file("SHA256", afilename);
}

char* styx_sha512_data(const char* data, long len){
	return hash_data("SHA512", data, (size_t)len);
}
char* styx_sha512_string(const char* astring){
	return hash_data("SHA512", astring, strlen(astring));
}
char* styx_sha512_file(const char* afilename){
	return hash_file("SHA512", afilename);
}

char* styx_ripemd160_data(const char* data, long len){
	return hash_data("RIPEMD160", data, (size_t)len);
}
char* styx_ripemd160_string(const char* astring){
	return hash_data("RIPEMD160", astring, strlen(astring));
}
char* styx_ripemd160_hexstring(const char* astring){
	return hash_hexstring("RIPEMD160", astring);
}
