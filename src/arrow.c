#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"
#include <time.h>
#include <stdlib.h>
#include <termios.h>

static struct termios oldt;

void restore_terminal_settings(void)
{
    tcsetattr(0, TCSANOW, &oldt);  /* Apply saved settings */
}

void disable_waiting_for_enter(void)
{
    struct termios newt;

    /* Make terminal read 1 char at a time */
    tcgetattr(0, &oldt);  /* Save terminal settings */
    newt = oldt;  /* Init new settings */
    newt.c_lflag &= ~(ICANON | ECHO);  /* Change settings */
    tcsetattr(0, TCSANOW, &newt);  /* Apply settings */
    atexit(restore_terminal_settings); /* Make sure settings will be restored when program ends  */
}

int main (int argc, char **argv)
{
	char *node = NULL;
	char *cookie = "42";
	char *mailb = "node_ipc_serv";
	int id = 1;
	if(argc == 3) {
		node = &argv[1][0];
		id = strtol(&argv[2][0], NULL, 10);
	} else {
		printf("1 argument needed\n");
		return 0;
	}
	int fd;	
	
	ETERM *resp;
	erl_init(NULL, 0);
	
	if (erl_connect_init(10 + id, cookie, 0) == -1)
		erl_err_quit("Error initiating connection");
	
	if ((fd = erl_connect(node)) < 0)
		erl_err_quit("Error connecting to node");
	
	int ch;
	int x = 104;
	int y = 123;
	char *name = "copter";
    disable_waiting_for_enter();

    while (1) {
        ch = getchar();        
		if(ch == 'a') {
			x -= 5;
			resp = erl_format("{publish,{taxi,~i,{~s,~i,~i}}}", id, name, x, y);
		}
		if(ch == 's') {
			y += 5;
			resp = erl_format("{publish,{taxi,~i,{~s,~i,~i}}}", id, name, x, y);
		}
		if(ch == 'd') {
			x += 5;
			resp = erl_format("{publish,{taxi,~i,{~s,~i,~i}}}", id, name, x, y);
		}
		if(ch == 'w') {
			y -= 5;
			resp = erl_format("{publish,{taxi,~i,{~s,~i,~i}}}", id, name, x, y);
		}
		printf("(X,Y) = (%d,%d)\n", x, y);
		erl_reg_send(fd, mailb, resp);
		erl_free_term(resp);
		}

}
