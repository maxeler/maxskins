CC              = g++
CXXFLAGS        = -I ../gen-cpp
LDFLAGS         = -lthrift

EXEC = Correlation_client

svc = ../gen-cpp/correlationService.o ../gen-cpp/correlation_constants.o ../gen-cpp/correlation_types.o
client = CorrelationClient.o

all: EXEC

EXEC:	$(svc) $(client)
	$(CC) $(CXXFLAGS) -o $(EXEC) $(svc) $(client) $(LDFLAGS)

clean:
	rm $(EXEC) $(svc) $(client)
