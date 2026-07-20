# builds libstyx — pluto's C companion (loaded by the styx system)
#
#   make            builds the shared library for this platform
#   make install    copies it to ~/.lisp (where styx.lisp looks)
#   make clean

UNAME_S := $(shell uname -s)

CC      ?= cc
CFLAGS  += -O2 -fPIC -Wall -Wextra
LDLIBS   = -lcrypto

ifeq ($(UNAME_S),Darwin)
    OPENSSL_PREFIX ?= $(shell brew --prefix openssl@3 2>/dev/null || echo /opt/homebrew/opt/openssl@3)
    CFLAGS  += -I$(OPENSSL_PREFIX)/include
    LDFLAGS += -L$(OPENSSL_PREFIX)/lib
    TARGET   = libstyx.dylib
else
    TARGET   = libstyx.so
endif

.PHONY: all install clean

all: $(TARGET)

$(TARGET): styx.c styx.h
	$(CC) $(CFLAGS) -shared -o $@ styx.c $(LDFLAGS) $(LDLIBS)

install: $(TARGET)
	mkdir -p ~/.lisp
	cp $(TARGET) ~/.lisp/

clean:
	rm -f libstyx.so libstyx.dylib styx.o
