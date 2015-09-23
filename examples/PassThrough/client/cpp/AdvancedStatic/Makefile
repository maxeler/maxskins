CC              = g++
CXXFLAGS        = -I ../gen-cpp
LDFLAGS         = -lthrift

EXEC = PassThrough_client

svc = ../gen-cpp/PassThroughService.o ../gen-cpp/PassThrough_constants.o ../gen-cpp/PassThrough_types.o
client = PassThroughClient.o

all: EXEC

EXEC:	$(svc) $(client)
	$(CC) $(CXXFLAGS) -o $(EXEC) $(svc) $(client) $(LDFLAGS)

clean:
	rm $(EXEC) $(svc) $(client)
