
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

#define SERVER_PORT 9999  
#define BUF_SIZE 200

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
	server_addr.sin_addr.s_addr = htonl(inet_addr(addr));

	return bind(sock,(struct sockaddr *)&server_addr,sizeof(server_addr));
}
	
int do_connect(int sock, char* addr){
	struct sockaddr_in server_addr;

	bzero(&server_addr,sizeof(server_addr));

	server_addr.sin_family = AF_INET;
	server_addr.sin_port = htons(SERVER_PORT);
	server_addr.sin_addr.s_addr = htonl(inet_addr(addr));

	return connect(sock,(struct sockaddr *)&server_addr,sizeof(server_addr));
}

int do_accept(int sock, char* addr){
	struct sockaddr_in server_addr;
	int addr_len = sizeof(server_addr);

	bzero(&server_addr,sizeof(server_addr));

	server_addr.sin_family = AF_INET;
	server_addr.sin_port = htons(SERVER_PORT);
	server_addr.sin_addr.s_addr = htonl(inet_addr(addr));

	return accept(sock,(struct sockaddr *)&server_addr,(socklen_t*)&addr_len);
}

char* do_recv(int sock){
	char recvBuf[BUF_SIZE];
	//printf("waiting for data ...\n");
	while(1){
		int dataNum = recv(sock, recvBuf, BUF_SIZE, 0);
		if(dataNum < 0){
			perror("recv");
			continue;
		}
		recvBuf[dataNum] = '\0';
		break;
	}
	char* data = recvBuf;
	//printf("%s received.\n",data);
	return data;
}


