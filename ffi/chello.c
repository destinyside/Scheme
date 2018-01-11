#include <sys/types.h>
#include <sys/un.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

void hello(){
    printf("Hello World!\n");
}

int max(int a,int b,int c){
    if(a>b){
	if(c>a){
	    return c;
	}
	else{
	    return a;
	}
    }
    else{
	if(c>b){
	    return c;
	}
	else{
	    return b;
	}
    }
}
