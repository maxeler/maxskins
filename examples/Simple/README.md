# Simple example

## Overview

This example demonstrates a simple example. dataOut[n] = dataIn[n] * dataIn[n] + dataIn[n] 

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

Create skin from **examples/Simple** directory with:

```bash
maxskins --py Simple.max
```

### Create skin for C++

Create skin from **examples/Simple** directory with:

```bash
maxskins --cpp Simple.max
```

### Create skin for Java

Create skin from **examples/Simple** directory with:

```bash
maxskins --java Simple.max
```
    
### Create skin for Ruby

Create skin from **examples/Simple** directory with:

```bash
maxskins --rb Simple.max
```
    
**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
In the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/Simple/server**) with:

```bash
./Simple_server 9090 &
```

### Run the Python example

Run the example from the **examples/Simple/client/py/BasicStatic/** or **examples/Simple/client/py/AdvancedStatic/** or **examples/Simple/client/py/Dynamic/** directory with:

```bash
./SimpleClient.py
```

### Run the C++ example

Run the example from the **examples/Simple/client/cpp/BasicStatic/** or **examples/Simple/client/cpp/AdvancedStatic/** or **examples/Simple/client/cpp/Dynamic/** directory with:

```bash
make
./Simple_client
```

### Run the Java example

Run the example from the **examples/Simple/client/java/BasicStatic/** or **examples/Simple/client/java/AdvancedStatic/** or **examples/Simple/client/java/Dynamic/** directory with:

```bash
ant
```

### Run the Ruby example

Run the example from the **examples/Simple/client/rb/BasicStatic/** or **examples/Simple/client/rb/AdvancedStatic/** or **examples/Simple/client/rb/Dynamic/** directory with:

```bash
./SimpleClient.rb
```
    
**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

```bash
maxcompilersim stop
```

