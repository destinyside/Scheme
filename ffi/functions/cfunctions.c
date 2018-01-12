#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

char* reverse(char* str){
	int len = strlen(str);
	char* tmp = (char*)malloc(sizeof(char)*len);
	for(int i = 0;i<len;i++){
		tmp[i]=str[len-i-1];
	}
	tmp[len]='\0';
	return tmp;
}

