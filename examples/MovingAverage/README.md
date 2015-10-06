# Moving Average example

## Overview

This example demonstrates moving average. dataOut[n] = (dataIn[n - 1] + dataIn[n] + dataIn[n + 1]) / 3

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

Create skin from **examples/MovingAverage** directory with:

```bash
maxskins --py MovingAverage.max
```

### Create skin for C++

Create skin from **examples/MovingAverage** directory with:

```bash
maxskins --cpp MovingAverage.max
```

### Create skin for C#

Create skin from **examples/MovingAverage** directory with:

```bash
maxskins --csharp MovingAverage.max
```

### Create skin for Java

Create skin from **examples/MovingAverage** directory with:

```bash
maxskins --java MovingAverage.max
```
    
### Create skin for Ruby

Create skin from **examples/MovingAverage** directory with:

```bash
maxskins --rb MovingAverage.max
```

### Create skin for PHP

Create skin from **examples/MovingAverage** directory with:

```bash
maxskins --php MovingAverage.max
```

**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
In the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/MovingAverage/server**) with:

```bash
./MovingAverage_server 9090 &
```

### Run the Python example

Run the example from the **examples/MovingAverage/client/py/BasicStatic/** or **examples/MovingAverage/client/py/AdvancedStatic/** or **examples/MovingAverage/client/py/Dynamic/** directory with:

```bash
./MovingAverageClient.py
```

### Run the C++ example

Run the example from the **examples/MovingAverage/client/cpp/BasicStatic/** or **examples/MovingAverage/client/cpp/AdvancedStatic/** or **examples/MovingAverage/client/cpp/Dynamic/** directory with:

```bash
make
./MovingAverage_client
```

### Run the C# example

Run the example from the **examples/MovingAverage/client/csharp/BasicStatic/** or **examples/MovingAverage/client/csharp/AdvancedStatic/** or **examples/MovingAverage/client/csharp/Dynamic/** directory with:

```bash
mcs /out:MovingAverageClient.exe MovingAverageClient.cs /recurse:../gen-csharp/com/maxeler/MovingAverage/*.cs /r:Thrift.dll
mono MovingAverageClient.exe
```

### Run the Java example

Run the example from the **examples/MovingAverage/client/java/BasicStatic/** or **examples/MovingAverage/client/java/AdvancedStatic/** or **examples/MovingAverage/client/java/AdvancedDynamic/** directory with:

```bash
ant
```

### Run the Ruby example

Run the example from the **examples/MovingAverage/client/rb/BasicStatic/** or **examples/MovingAverage/client/rb/AdvancedStatic/** or **examples/MovingAverage/client/rb/Dynamic/** directory with:

```bash
./MovingAverageClient.rb
```

### Run the PHP example

Run the example from the **examples/MovingAverage/client/php/BasicStatic/** or **examples/MovingAverage/client/php/Dynamic/** directory with:

```bash
php ./MovingAverageClient.php
```
    
**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

```bash
maxcompilersim stop
```

