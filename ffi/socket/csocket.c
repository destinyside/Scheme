
#include <sys/stat.h>  
#include <fcntl.h>  
#include <errno.h>  
#include <netdb.h>  
#include <sys/types.h>  
#include <sys/socket.h>  
#include <netinet/in.h>  
#include <arpa/inet.h>  

#include <stdio.h>  
#include <string.h>  
#include <stdlib.h>  
#include <unistd.h>  

#define SERVER_PORT 5555  

//int send(SOCKET s, const char* buf, int len, int flags);
//int recv(SOCKET s, char* buf, int len, int flags);

int do_error(int state, char* message){
	if(state < 0){
		perror(message);
		return 1;
	}
}

int do_socket(void){
	return socket(AF_INET,SOCK_STREAM,0);
}

int do_bind(int sock, char* addr){
	struct sockaddr_in server_addr;

	bzero(&server_addr,sizeof(server_addr));

	server_addr.sin_family = AF_INET;
	server_addr.sin_port = htons(SERVER_PORT);
	server_addr.sin_addr.s_addr = htonl(addr);

	return bind(sock,(struct sockaddr *)&server_addr,sizeof(server_addr));
}
	
int do_connect(int sock, char* addr){
	struct sockaddr_in server_addr;

	bzero(&server_addr,sizeof(server_addr));

	server_addr.sin_family = AF_INET;
	server_addr.sin_port = htons(SERVER_PORT);
	server_addr.sin_addr.s_addr = htonl(addr);

	return connect(sock,(struct sockaddr *)&server_addr,sizeof(server_addr));
}

int do_accept(int sock){
	struct sockaddr_in server_addr;

	bzero(&server_addr,sizeof(server_addr));

	server_addr.sin_family = AF_INET;
	server_addr.sin_port = htons(SERVER_PORT);
	server_addr.sin_addr.s_addr = htonl(INADDR_ANY);

	return accept(sock,(struct sockaddr *)&server_addr,sizeof(server_addr));
}
