

CC=cc
CFLAGS=-fPIC -shared
#INCLUDE= -I/usr/include

LIBDIR=/usr/lib

SOURCE_DIR=../../../ffi/socket

SRC=csocket.c
TARGET=csocket.so
SCM=socket.scm
ORIGIN_PWD=$(PWD)
Scheme=/usr/bin/scheme -q
build: 
	(cd $(ORIGIN_PWD)/$(SOURCE_DIR) && $(MAKE) clean && $(MAKE) && cp *.so $(ORIGIN_PWD))
	(cd $(ORIGIN_PWD))

clean:
	rm -f *.so
