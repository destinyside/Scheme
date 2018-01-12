

CC=cc
CFLAGS=-fPIC -shared
#INCLUDE= -I/usr/include

LIBDIR=/usr/lib

SRC=csocket.c
TARGET=csocket.so
SCM=socket.scm

Scheme=/usr/bin/scheme -q
build: $(SRC)
	$(CC) $(CFLAGS) $(INCLUDE) -o $(TARGET) $(SRC)
#	install $(TARGET) $(LIBDIR)
	echo '(time (compile-file "$(SCM)"))' | $(Scheme)

