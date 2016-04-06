# LMemLoopback example

## Overview

Adds two LMem input streams and writes the result to LMem. 

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

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --py LMemLoopback.max
```

### Create skin for C++

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --cpp LMemLoopback.max
```

### Create skin for Java

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --java LMemLoopback.max
```
    
### Create skin for Ruby

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --rb LMemLoopback.max
```

### Create skin for C# 

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --csharp LMemLoopback.max
```

### Create skin for Go

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --go LMemLoopback.max
```

### Create skin for Perl

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --perl LMemLoopback.max
```

### Create skin for PHP 

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --php LMemLoopback.max
```

### Create skin for Haskell

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --hs LMemLoopback.max
```

### Create skin for Erlang

Create skin from **examples/LMemLoopback** directory with:

```bash
maxskins --erl LMemLoopback.max
```
    
**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
In the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/LMemLoopback/server**) with:

```bash
./LMemLoopback_server 9090 &
```

### Run the Python example

Run the example from the **examples/LMemLoopback/client/py/BasicStatic/** or **examples/LMemLoopback/client/py/AdvancedStatic/** directory with:

```bash
./LMemLoopbackClient.py
```

### Run the C++ example

Run the example from the **examples/LMemLoopback/client/cpp/BasicStatic/** or **examples/LMemLoopback/client/cpp/AdvancedStatic/** or **examples/LMemLoopback/client/cpp/Dynamic/** directory with:

```bash
make
./LMemLoopback_client
```

### Run the Java example

Run the example from the **examples/LMemLoopback/client/java/BasicStatic/** or **examples/LMemLoopback/client/java/AdvancedStatic/** or **examples/LMemLoopback/client/java/Dynamic/** directory with:

```bash
ant
```

### Run the Ruby example

Run the example from the **examples/LMemLoopback/client/rb/BasicStatic/** or **examples/LMemLoopback/client/rb/AdvancedStatic/** or **examples/LMemLoopback/client/rb/Dynamic/** directory with:

```bash
./LMemLoopbackClient.rb
```

### Run the C# example

Run the example from the **examples/LMemLoopback/client/csharp/BasicStatic/** or **examples/LMemLoopback/client/csharp/AdvancedStatic/** or **examples/LMemLoopback/client/csharp/Dynamic/** directory with:

```bash
mcs /out:LMemLoopbackClient.exe LMemLoopbackClient.cs /recurse:../gen-csharp/com/maxeler/LMemLoopback/*.cs /r:$MONO_PATH/Thrift.dll
mono LMemLoopbackClient.exe
```

### Run the Go example

Run the example from the **examples/LMemLoopback/client/go/BasicStatic/** or **examples/LMemLoopback/client/go/AdvancedStatic/** or **examples/LMemLoopback/client/go/Dynamic/** directory with:

```bash
go run LMemLoopbackClient.go
```

### Run the Perl example

Run the example from the **examples/LMemLoopback/client/perl/BasicStatic/** or **examples/LMemLoopback/client/perl/AdvancedStatic/** or **examples/LMemLoopback/client/perl/Dynamic/** directory with:

```bash
perl LMemLoopbackClient.pl
```

### Run the PHP example

Run the example from the **examples/LMemLoopback/client/php/BasicStatic/** or **examples/LMemLoopback/client/php/Dynamic/** directory with:

```bash
php LMemLoopbackClient.php
```

### Run the Haskell example

Run the example from the **examples/LMemLoopback/client/hs/BasicStatic/** or **examples/LMemLoopback/client/hs/AdvancedStatic/** or **examples/LMemLoopback/client/hs/Dynamic/** directory with:

```bash
ghc -i$HASKELLPATH -o LMemLoopbackClient LMemLoopbackClient.hs
./LMemLoopbackClient
```

### Run the Erlang example

Run the example from the **examples/LMemLoopback/client/erl/BasicStatic/** or **examples/LMemLoopback/client/erl/AdvancedStatic/** or **examples/LMemLoopback/client/erl/Dynamic/** directory with:

```bash
erlc -I $EINCLUDEPATH -I $EBINPATH -I ../gen-erl/ -o ../gen-erl/ ../gen-erl/*.erl
erlc -I $EINCLUDEPATH -I ../gen-erl/ lMemLoopbackClient.erl
erl -pa $EBINPATH -pa ../gen-erl/ -noshell -s lMemLoopbackClient t -s init stop
```

**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

```bash
maxcompilersim stop
```

