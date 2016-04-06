# Pass Through example

## Overview

This example demonstrates passing through. dataOut[n] = dataIn[n]

## Running this example

This DFE is compiled for VECTIS card using MaxCompiler 2014.2.

### Starting the simulator

To run it using simulator, first simulated VECTIS card needs to be (re)started:

```bash
maxcompilersim restart
```

These environment variables need to be exported so this example can be executed:

```bash
export MAXELEROSDIR=$MAXCOMPILERDIR/lib/maxeleros-sim
export LD_PRELOAD=$MAXELEROSDIR/lib/libmaxeleros.so:$LD_PRELOAD
export SLIC_CONF="$SLIC_CONF;use_simulation=sim"
```

### Create skin for Python

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --py PassThrough.max
```

### Create skin for C++

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --cpp PassThrough.max
```

### Create skin for Java

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --java PassThrough.max
```
    
### Create skin for Ruby

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --rb PassThrough.max
```

### Create skin for C# 

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --csharp PassThrough.max
```

### Create skin for Go

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --go PassThrough.max
```

### Create skin for Perl

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --perl PassThrough.max
```

### Create skin for PHP

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --php PassThrough.max
```

### Create skin for Erlang

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --erl PassThrough.max
```

### Create skin for Haskell

Create skin from **examples/PassThrough** directory with:

```bash
maxskins --hs PassThrough.max
```
    
**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
In the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/PassThrough/server**) with:

```bash
./PassThrough_server 9090 &
```

### Run the Python example

Run the example from the **examples/PassThrough/client/py/BasicStatic/** or **examples/PassThrough/client/py/AdvancedStatic/** or **examples/PassThrough/client/py/Dynamic/** directory with:

```bash
./PassThroughClient.py
```

### Run the C++ example

Run the example from the **examples/PassThrough/client/cpp/BasicStatic/** or **examples/PassThrough/client/cpp/AdvancedStatic/** or **examples/PassThrough/client/cpp/Dynamic/** directory with:

```bash
make
./PassThrough_client
```

### Run the Java example

Run the example from the **examples/PassThrough/client/java/BasicStatic/** or **examples/PassThrough/client/java/AdvancedStatic/** or **examples/PassThrough/client/java/Dynamic/** directory with:

```bash
ant
```

### Run the Ruby example

Run the example from the **examples/PassThrough/client/rb/BasicStatic/** or **examples/PassThrough/client/rb/AdvancedStatic/** or **examples/PassThrough/client/rb/Dynamic/** directory with:

```bash
./PassThroughClient.rb
```

### Run the C# example

Run the example from the **examples/PassThrough/client/csharp/BasicStatic/** or **examples/PassThrough/client/csharp/AdvancedStatic/** or **examples/PassThrough/client/csharp/Dynamic/** directory with:

```bash
mcs /out:PassThroughClient.exe PassThroughClient.cs /recurse:../gen-csharp/com/maxeler/PassThrough/*.cs /r:$MONO_PATH/Thrift.dll
mono PassThroughClient.exe
```

### Run the Go example

Run the example from the **examples/PassThrough/client/go/BasicStatic/** or **examples/PassThrough/client/go/AdvancedStatic/** or **examples/PassThrough/client/go/Dynamic/** directory with:

```bash
go run PassThroughClient.go
```

### Run the Perl example

Run the example from the **examples/PassThrough/client/perl/BasicStatic/** or **examples/PassThrough/client/perl/AdvancedStatic/** or **examples/PassThrough/client/perl/Dynamic/** directory with:

```bash
perl PassThroughClient.pl
```

### Run the PHP example

Run the example from the **examples/PassThrough/client/php/BasicStatic/** or **examples/PassThrough/client/php/Dynamic/** directory with:

```bash
php PassThroughClient.php
```

### Run the Erlang example

Run the example from the **examples/PassThrough/client/erl/BasicStatic/** or **examples/PassThrough/client/erl/AdvancedStatic/** or **examples/PassThrough/client/erl/Dynamic/** directory with:

```bash
erlc -I $EINCLUDEPATH -I $EBINPATH -I ../gen-erl/ -o ../gen-erl/ ../gen-erl/*.erl
erlc -I $EINCLUDEPATH -I ../gen-erl/ passThroughClient.erl
erl -pa $EBINPATH -pa ../gen-erl/ -noshell -s passThroughClient t -s init stop
```

### Run the Haskell example

Run the example from the **examples/PassThrough/client/hs/BasicStatic/** or **examples/PassThrough/client/hs/AdvancedStatic/** or **examples/PassThrough/client/hs/Dynamic/** directory with:

```bash
ghc -i$HASKELLPATH -o PassThroughClient PassThroughClient.hs
./PassThroughClient
```
    
**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

```bash
maxcompilersim stop
```

