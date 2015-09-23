#include <arpa/inet.h>
#include <linux/if.h>
#include <linux/if_ether.h>
#include <netinet/ether.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>



static int create_socket(struct in_addr *remote_ip, int port) {
	int sock = socket(AF_INET, SOCK_DGRAM, 0);

	struct sockaddr_in sockaddr;
	memset(&sockaddr, 0, sizeof(sockaddr));
	sockaddr.sin_family = AF_INET;
	sockaddr.sin_port = htons(port);

//	sockaddr.sin_addr = *local_ip;
	if (bind(sock, (struct sockaddr *)&sockaddr, sizeof(sockaddr)) != 0) {
		printf("Bind failed: %s\n", strerror(errno));
		exit(1);
	}

	sockaddr.sin_addr = *remote_ip;
    connect(sock, (const struct sockaddr*) &sockaddr, sizeof(sockaddr));

	return sock;
}


// The hardware will accept a variable sized frame, but my test is simple...

typedef struct __attribute__((packed)) {
	uint8_t aSz : 3;
	uint8_t bSz : 3;
	uint8_t cSz : 2;
	uint64_t a : 24;
	uint64_t b : 64;
	uint64_t c : 8;
} something1_t;

typedef struct __attribute__((packed)) {
	uint8_t aSz : 3;
	uint8_t bSz : 3;
	uint8_t cSz : 2;
	uint64_t a : 48;
	uint64_t b : 8;
	uint64_t c : 32;
} something2_t;

static void send_frames(int sock) {
	something1_t *s1 = malloc(sizeof(something1_t));
	something2_t *s2 = malloc(sizeof(something2_t));
	
	s1->aSz = 3;
	s1->bSz = 0;
	s1->cSz = 1;

	s1->a = 0x800201;
	s1->b = 0x7766554433221100UL;
	s1->c = 0x80;


	s2->aSz = 6;
	s2->bSz = 1;
	s2->cSz = 3;

	s2->a = 0x800102030405UL;
	s2->b = 0x78;
	s2->c = 0x818283;
	
	send(sock, s1, sizeof(something1_t), MSG_WAITALL);
	usleep(1000);
	send(sock, s2, sizeof(something2_t), MSG_WAITALL);
}


int main(int argc, char *argv[]) {
	(void) argc;
	(void) argv;

	struct in_addr remote_ip;
	inet_aton("172.16.50.1", &remote_ip);

	int mySocket = create_socket(&remote_ip, 2000);

	send_frames(mySocket);

	printf("Sender finished\n");
}

