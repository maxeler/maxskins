CC              = g++
CXXFLAGS        = -I ../gen-cpp
LDFLAGS         = -lthrift

EXEC = SignExt_client

svc = ../gen-cpp/SignExtService.o ../gen-cpp/SignExt_constants.o ../gen-cpp/SignExt_types.o
client = SignExt.o

all: EXEC

EXEC:	$(svc) $(client)
	$(CC) $(CXXFLAGS) -o $(EXEC) $(svc) $(client) $(LDFLAGS)

clean:
	rm $(EXEC) $(svc) $(client)
