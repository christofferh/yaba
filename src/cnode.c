#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"
#include <time.h>

//
// Compile: gcc -o fac -I/opt/local/lib/erlang/lib/erl_interface-3.7.2/include -L/opt/local/lib/erlang/lib/erl_interface-3.7.2/lib fac.c -lerl_interface -lei
// Run: ./cnode node topic id data
// E.g: ./cnode christ@student-245-77.eduroam.uu.se taxi 1 taxi1.txt
//

int main (int argc, char **argv)
{
	char *cookie = "42";
	char *mailb = "node_ipc_serv";
	char *node = NULL;
	char *inputfile = NULL;
	char *topic = NULL;
	int id = 0;
	int fd;
	
	if(argc == 5) {
		node = &argv[1][0];
		topic = &argv[2][0];
		id = strtol(&argv[3][0], NULL, 10);
		inputfile = &argv[4][0];
	} else {
		printf("4 arguments needed\n");
		return 0;
	}
	
	ETERM *resp;
	erl_init(NULL, 0);
	
	if (erl_connect_init(id, cookie, 0) == -1)
		erl_err_quit("Error initiating connection");
	
	if ((fd = erl_connect(node)) < 0)
		erl_err_quit("Error connecting to node");
	
	while(1) {
	FILE* fp = fopen(inputfile,"rt");
	char name[20];
	int x, y;
	char line[100];
	
		
	while(fgets(line, 100, fp) != NULL)
		{
			sscanf (line, "%s %d %d\n", name, &x, &y);
			resp = erl_format("{publish,{~a,~i,{~s,~i,~i}}}", topic, id, name, x, y);
			erl_reg_send(fd, mailb, resp);
			printf("[%s:%d:%s] (X,Y) = (%d,%d)\n", topic, id, name, x, y);
			erl_free_term(resp);
			usleep(1000000);
	  	}
	fclose(fp);
	}
}
