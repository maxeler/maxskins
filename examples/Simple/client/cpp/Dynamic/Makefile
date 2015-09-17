CC              = g++
CXXFLAGS        = -I ../gen-cpp
LDFLAGS         = -lthrift

EXEC = Simple_client

svc = ../gen-cpp/SimpleService.o ../gen-cpp/Simple_constants.o ../gen-cpp/Simple_types.o
client = SimpleClient.o

all: EXEC

EXEC:	$(svc) $(client)
	$(CC) $(CXXFLAGS) -o $(EXEC) $(svc) $(client) $(LDFLAGS)

clean:
	rm $(EXEC) $(svc) $(client)
