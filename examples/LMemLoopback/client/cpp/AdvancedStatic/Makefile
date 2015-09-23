CC              = g++
CXXFLAGS        = -I ../gen-cpp
LDFLAGS         = -lthrift

EXEC = LMemLoopback_client

svc = ../gen-cpp/LMemLoopbackService.o ../gen-cpp/LMemLoopback_constants.o ../gen-cpp/LMemLoopback_types.o
client = LMemLoopbackClient.o

all: EXEC

EXEC:	$(svc) $(client)
	$(CC) $(CXXFLAGS) -o $(EXEC) $(svc) $(client) $(LDFLAGS)

clean:
	rm $(EXEC) $(svc) $(client)
