CC              = g++
CXXFLAGS        = -I ../gen-cpp
LDFLAGS         = -lthrift

EXEC = MovingAverage_client

svc = ../gen-cpp/MovingAverageService.o ../gen-cpp/MovingAverage_constants.o ../gen-cpp/MovingAverage_types.o
client = MovingAverageClient.o

all: EXEC

EXEC:	$(svc) $(client)
	$(CC) $(CXXFLAGS) -o $(EXEC) $(svc) $(client) $(LDFLAGS)

clean:
	rm $(EXEC) $(svc) $(client)
