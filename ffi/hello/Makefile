

CC=cc
CFLAGS=-fPIC -shared
#INCLUDE= -I/usr/include

LIBDIR=/usr/lib

TARGET=chello.so
Scheme=/usr/bin/scheme -q
build: chello.c
	$(CC) $(CFLAGS) $(INCLUDE) -o $(TARGET) chello.c
#	install $(TARGET) $(LIBDIR)
	echo '(time (compile-file "hello.scm"))' | $(Scheme)

