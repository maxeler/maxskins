# Correlation example

## Overview

Statistical method to analyze the relationship between two random variables.

## Running this example

This DFE is compiled for ISCA card using MaxCompiler 2014.2.

### Starting the simulator

To run it using simulator, first simulated ISCA card needs to be (re)started:

```bash
maxcompilersim -c ISCA restart
```

These environment variables need to be exported so this example can be executed:

```bash
export MAXELEROSDIR=$MAXCOMPILERDIR/lib/maxeleros-sim
export LD_PRELOAD=$MAXELEROSDIR/lib/libmaxeleros.so:$LD_PRELOAD
export SLIC_CONF="$SLIC_CONF;use_simulation=sim"
```

### Create skin for Python

Create skin from **examples/Correlation** directory with:

```bash
maxskins --py correlation.max
```

### Create skin for C++

Create skin from **examples/Correlation** directory with:

```bash
maxskins --cpp correlation.max
```

### Create skin for C# 

Create skin from **examples/Correlation** directory with:

```bash
maxskins --csharp correlation.max
```

### Create skin for Java

Create skin from **examples/Correlation** directory with:

```bash
maxskins --java correlation.max
```

### Create skin for Ruby
     
Create skin from **examples/Correlation** directory with:

```bash
maxskins --rb correlation.max
```

### Create skin for Go

Create skin from **examples/Correlation** directory with:

```bash
maxskins --go correlation.max
```

### Create skin for Haskell

Create skin from **examples/Correlation** directory with:

```bash
maxskins --hs correlation.max
```

**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
In the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/Correlation/server**) with:

```bash
./correlation_server 9090 &
```

### Run the Python example

Run the example from the **examples/Correlation/client/py/BasicStatic/** or **examples/Correlation/client/py/AdvancedStatic/** or **examples/Correlation/client/py/Dynamic/** directory with:

```bash
./CorrelationClient.py <stream size> <number of streams>
```

### Run the C++ example

Run the example from the **examples/Correlation/client/cpp/BasicStatic/** or **examples/Correlation/client/cpp/AdvancedStatic/** or **examples/Correlation/client/cpp/Dynamic/** directory with:

```bash
make
./correlation_client <stream size> <number of streams>
```

### Run the C# example

Run the example from the **examples/Correlation/client/csharp/BasicStatic/** or **examples/Correlation/client/csharp/AdvancedStatic/** or **examples/Correlation/client/csharp/Dynamic/** directory with:

```bash
mcs /out:CorrelationClient.exe CorrelationClient.cs /recurse:../gen-csharp/com/maxeler/correlation/*.cs /r:$MONO_PATH/Thrift.dll
mono CorrelationClient.exe <stream size> <number of streams>
```

### Run the java example

Run the example from the **examples/Correlation/client/java/BasicStatic/** or **examples/Correlation/client/java/AdvancedStatic/** or **examples/Correlation/client/java/Dynamic/** directory with:

```bash
ant -DstreamSize=<stream size> -DnumberOfStreams=<number of streams>
```

### Run the Ruby example

Run the example from the **examples/Correlation/client/rb/BasicStatic/** or **examples/Correlation/client/rb/AdvancedStatic/** or **examples/Correlation/client/rb/Dynamic/** directory with:

```bash
./CorrelationClient.rb <stream size> <number of streams>
```

### Run the Go example

Run the example from the **examples/Correlation/client/go/BasicStatic/** or **examples/Correlation/client/go/AdvancedStatic/** or **examples/Correlation/client/go/Dynamic/** directory with:

```bash
go run CorrelationClient.go <stream size> <number of streams>
```

### Run the Haskell example

Run the example from the **examples/Correlation/client/hs/BasicStatic/** or **examples/Correlation/client/hs/AdvancedStatic/** or **examples/Correlation/client/hs/Dynamic/** directory with:

```bash
ghc -i$HASKELLPATH -O2  CorrelationClient.hs -o CorrelationClient
./CorrelationClient <stream size> <number of streams>
```

**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

```bash
maxcompilersim -c ISCA stop
```

