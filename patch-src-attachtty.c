--- attachtty.c.orig	2003-08-04 12:32:23.000000000 +0300
+++ attachtty.c	2006-03-18 20:55:12.000000000 +0300
@@ -1,9 +1,10 @@
 #include "config.h"
+#include "time.h"
 
 extern FILE *log_fp;
 int copy_a_bit(int in_fd, int out_fd, int dribble_fd,char *message) ;
-void connect_direct(char * path) ;
-void connect_ssh(char *host, char *path) ;
+void connect_direct(char * path, char *cmd, int timeout) ;
+void connect_ssh(char *host, char *path, char *cmd) ;
 
 #define UNIX_PATH_MAX 108
 
@@ -32,10 +33,12 @@
     char *host=NULL;		/* "hostname" or "user@hostname" */
     char *path;			/* path to socket */
     char *p;
+    char *cmd=NULL;
+    int timeout = 1;
     struct  sigaction  act;
   
-    if(argc!=2) {
-	fprintf(stderr, "%s: unrecognized arguments\nusage: %s /path/to/socket\n       %s remote_user@remote_host:/path/to/socket\n", argv[0],  argv[0], argv[0]);
+    if(argc<2 || argc>4) {
+	fprintf(stderr, "%s: unrecognized arguments\nusage: %s /path/to/socket [cmd] [timeout]\n       %s remote_user@remote_host:/path/to/socket [cmd]\n", argv[0],  argv[0], argv[0]);
 	exit(1);
     }
     p=strdup(argv[1]);
@@ -47,6 +50,13 @@
     } else {
 	path=p;
     }
+    if (argc >= 3)
+        cmd = argv[2];
+    if (argc == 4) {
+        int read_timeout = atoi(argv[3]);
+        if (read_timeout > 0)
+            timeout = read_timeout;
+    }
 
     /* catch SIGINT and send character \003 over the link */
     act.sa_handler=control_c_pressed;
@@ -62,16 +72,16 @@
 
     if(host) {
 	logprintf("attachtty","connecting through ssh to %s on %s\n",path,host);
-	connect_ssh(host,path);
+	connect_ssh(host,path,cmd);
     } else {
 	logprintf("attachtty","connecting directly to %s\n",path);
-	connect_direct(path);
+	connect_direct(path,cmd,timeout);
     }
 }
 
 /* copy between stdin,stdout and unix-domain socket */
 
-void connect_direct(char * path) {
+void connect_direct(char * path, char *cmd, int timeout) {
     int sock=-1;
     struct pollfd ufds[3];
     struct sockaddr_un s_a;
@@ -86,28 +96,62 @@
     if(connect(sock,(const struct sockaddr *) &s_a,sizeof s_a)!=0) 
 	bail("attachtty","connect");
   
-    while(! time_to_die) {
-	if(was_interrupted) {
-	    write(sock,"\003",1);
-	    was_interrupted=0;
-	}
-	ufds[0].fd=sock; ufds[0].events=POLLIN;
-	ufds[1].fd=0; ufds[1].events=POLLIN|POLLHUP;
-	if (poll(ufds,2 ,-1) == -1) continue;
-	if(ufds[0].revents & POLLIN) 
-	    copy_a_bit(sock,1,-1,"copying from socket");
+    if (cmd) {
+        int time_start = time(NULL);
+        int time_end = time_start + timeout;
+        ufds[0].fd=sock; ufds[0].events=POLLIN | POLLOUT;
+
+        while(! time_to_die) {
+            if(was_interrupted) {
+                write(sock,"\003",1);
+                was_interrupted=0;
+            }
+            int msec_left = (time_end - time(NULL)) * 1000;  
+            if (poll(ufds, 1 , msec_left) == -1) continue;
+            if(ufds[0].revents & POLLIN) 
+                copy_a_bit(sock,1,-1,"copying from socket");
+	
+            if(cmd && (ufds[0].revents & POLLOUT)) {
+                int len = strlen(cmd);
+                int written = write(sock,cmd,strlen(cmd));
+                if (written == len) {
+                    ufds[0].events = POLLIN; /* no longer need to output */
+                    cmd = NULL;
+                    write(sock,"\012",1);
+                } else 
+                    cmd += written;
+            }
+
+            if (time(NULL) >= time_end) {
+                close(sock);
+                sock=-1;
+                time_to_die = 1;
+            }
+        }
+    } else {
+        while(! time_to_die) {
+            if(was_interrupted) {
+                write(sock,"\003",1);
+                was_interrupted=0;
+            }
+            ufds[0].fd=sock; ufds[0].events=POLLIN;
+            ufds[1].fd=0; ufds[1].events=POLLIN|POLLHUP;
+            if (poll(ufds,2 ,-1) == -1) continue;
+            if(ufds[0].revents & POLLIN) 
+                copy_a_bit(sock,1,-1,"copying from socket");
     
-	if(ufds[1].revents & POLLIN) {	
-	    int n=copy_a_bit(0,sock,-1,"copying to socket");
-	    if(n==0) {
-		logprintf("attachtty","closed connection due to zero-length read");
-		close(sock); sock=-1;
-	    }
-	}
-	if(ufds[1].revents & POLLHUP) {
-	    logprintf("attachtty","closed connection due to hangup");
-	    exit(0);
-	}
+            if(ufds[1].revents & POLLIN) {	
+                int n=copy_a_bit(0,sock,-1,"copying to socket");
+                if(n==0) {
+                    logprintf("attachtty","closed connection due to zero-length read");
+                    close(sock); sock=-1;
+                }
+            }
+            if(ufds[1].revents & POLLHUP) {
+                logprintf("attachtty","closed connection due to hangup");
+                exit(0);
+            }
+        }
     }
 }
 
@@ -127,7 +171,7 @@
 /* we do character-at-a-time copying into ssh, because speed is not 
    critical and I'm lazy */
 
-void connect_ssh(char *host, char *path) {
+void connect_ssh(char *host, char *path, char *cmd) {
     int pipe_des[2];
     int pid;
     char buf[2];
@@ -141,7 +185,7 @@
 	setsid();
 	close(0);
 	dup2(pipe_des[0],0);
-	execlp("ssh", "ssh", host, "attachtty", path, NULL);
+        execlp("ssh", "ssh", host, "attachtty", cmd,path, NULL);
 	bail("attachtty", "exec failed");
     } else {			/* parent */
 	logprintf("attachtty","Successfully started"); 
@@ -165,3 +209,9 @@
 	/* don't worry about wait()ing for it, we're leaving anyway */
     }
 }
+
+/* 
+ * Local Variables:
+ * c-basic-offset: 4
+ * End:
+ */
