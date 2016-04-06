# Vector Addition example

## Overview

This example demonstrates addition of two vectors and a scalar. c[n] = a[n] + b[n] + scalar

First vector is first copied to LMem, and it streams through DFE with another vector which is streamed from the host. The output vector is streame back to host.

## Running this example

This DFE is compiled for MAIA card using MaxCompiler 2014.2.

### Starting the simulator

To run it using simulator, first simulated MAIA card needs to be (re)started:

```bash
maxcompilersim -c MAIA restart
```

These environment variables need to be exported so this example can be executed:

```bash
export MAXELEROSDIR=$MAXCOMPILERDIR/lib/maxeleros-sim
export LD_PRELOAD=$MAXELEROSDIR/lib/libmaxeleros.so:$LD_PRELOAD
export SLIC_CONF="$SLIC_CONF;use_simulation=sim"
```

### Create skin for Python

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --py VectorAddition.max
```

### Create skin for C++

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --cpp VectorAddition.max
```

### Create skin for Java

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --java VectorAddition.max
```

### Create skin for Ruby

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --rb VectorAddition.max
```

### Create skin for Go

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --go VectorAddition.max
```

### Create skin for C# 

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --csharp VectorAddition.max
```
  
### Create skin for PHP

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --php VectorAddition.max
```

### Create skin for Haskell

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --hs VectorAddition.max
```

### Create skin for Erlang

Create skin from **examples/VectorAddition** directory with:

```bash
maxskins --erl VectorAddition.max
```
    
**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
In the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/VectorAddition/server**) with:

```bash
./VectorAddition_server 9090 &
```

### Run the Python example

Run the example from the **examples/VectorAddition/client/py/BasicStatic/** or **examples/VectorAddition/client/py/AdvancedStatic/** or **examples/VectorAddition/client/py/Dynamic/** directory with:

```bash
./VectorAdditionClient.py
```

### Run the C++ example

Run the example from the **examples/VectorAddition/client/cpp/BasicStatic/** or **examples/VectorAddition/client/cpp/AdvancedStatic/** or **examples/VectorAddition/client/cpp/Dynamic/** directory with:

```bash
make
./VectorAddition_client
```

### Run the Java example

Run the example from the **examples/VectorAddition/client/java/BasicStatic/** or **examples/VectorAddition/client/java/AdvancedStatic/** or **examples/VectorAddition/client/java/Dynamic/** directory with:

```bash
ant
```

### Run the Ruby example

Run the example from the **examples/VectorAddition/client/rb/BasicStatic/** or **examples/VectorAddition/client/rb/AdvancedStatic/** or **examples/VectorAddition/client/rb/Dynamic/** directory with:

```bash
./VectorAdditionClient.rb
```


### Run the Go example

Run the example from the **examples/VectorAddition/client/go/BasicStatic/** or **examples/VectorAddition/client/go/AdvancedStatic/** or **examples/VectorAddition/client/go/Dynamic/** directory with:

```bash
go run VectorAdditionClient.go
```

### Run the C# example

Run the example from the **examples/VectorAddition/client/csharp/BasicStatic/** or **examples/VectorAddition/client/csharp/AdvancedStatic/** or **examples/VectorAddition/client/csharp/Dynamic/** directory with:

```bash
mcs /out:VectorAdditionClient.exe VectorAdditionClient.cs /recurse:../gen-csharp/com/maxeler/VectorAddition/*.cs /r:$MONO_PATH/Thrift.dll
mono VectorAdditionClient.exe
```

### Run the PHP example

Run the example from the **examples/VectorAddition/client/php/BasicStatic/** or **examples/VectorAddition/client/php/Dynamic/** directory with:

```bash
php ./VectorAdditionClient.php
```

### Run the Haskell example

Run the example from the **examples/VectorAddition/client/hs/BasicStatic/** or **examples/VectorAddition/client/hs/AdvancedStatic/** or **examples/VectorAddition/client/hs/Dynamic/** directory with:

```bash
ghc -i$HASKELLPATH -o VectorAdditionClient VectorAdditionClient.hs
./VectorAdditionClient
```

### Run the Erlang example

Run the example from the **examples/VectorAddition/client/erl/BasicStatic/** or **examples/VectorAddition/client/erl/AdvancedStatic/** or **examples/VectorAddition/client/erl/Dynamic/** directory with:

```bash
erlc -I $EINCLUDEPATH -I $EBINPATH -I ../gen-erl/ -o ../gen-erl/ ../gen-erl/*.erl
erlc -I $EINCLUDEPATH -I ../gen-erl/ vectorAdditionClient.erl
erl -pa $EBINPATH -pa ../gen-erl/ -noshell -s vectorAdditionClient t -s init stop
```
    
**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

```bash
maxcompilersim -c MAIA stop
```
