CC              = g++
CXXFLAGS        = -I ../gen-cpp
LDFLAGS         = -lthrift

EXEC = VectorAddition_client

svc = ../gen-cpp/VectorAdditionService.o ../gen-cpp/VectorAddition_constants.o ../gen-cpp/VectorAddition_types.o
client = VectorAdditionClient.o

all: EXEC

EXEC:	$(svc) $(client)
	$(CC) $(CXXFLAGS) -o $(EXEC) $(svc) $(client) $(LDFLAGS)

clean:
	rm $(EXEC) $(svc) $(client)
