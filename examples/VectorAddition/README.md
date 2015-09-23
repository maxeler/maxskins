# Vector Addition example

## Overview

This example demonstrates addition of two vectors and a scalar. c[n] = a[n] + b[n] + scalar

First vector is first copied to LMem, and it streams through DFE with another vector which is streamed from the host. The output vector is streame back to host.

## Running this example

This DFE is compiled for MAIA card using MaxCompiler 2014.2.

### Starting the simulator

To run it using simulator, first simulated MAIA card needs to be (re)started:

    maxcompilersim -c MAIA restart

These environment variables need to be exported so this example can be executed:

    export MAXELEROSDIR=$MAXCOMPILERDIR/lib/maxeleros-sim
    export LD_PRELOAD=$MAXELEROSDIR/lib/libmaxeleros.so:$LD_PRELOAD
    export SLIC_CONF="$SLIC_CONF;use_simulation=sim"

### Create skin for Python

Create skin from **examples/VectorAddition** directory with:

    maxskins --py VectorAddition.max

### Create skin for C++

Create skin from **examples/VectorAddition** directory with:

    maxskins --cpp VectorAddition.max

### Create skin for Java

Create skin from **examples/VectorAddition** directory with:

    maxskins --java VectorAddition.max

### Create skin for Ruby

Create skin from **examples/VectorAddition** directory with:

    maxskins --rb VectorAddition.max
    
**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
In the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/VectorAddition/server**) with:

    ./VectorAddition_server 9090 &

### Run the Python example

Run the example from the **examples/VectorAddition/client/py/BasicStatic/** or **examples/VectorAddition/client/py/AdvancedStatic/** or **examples/VectorAddition/client/py/AdvancedDynamic/** directory with:

    ./VectorAdditionClient.py

### Run the C++ example

Run the example from the **examples/VectorAddition/client/cpp/BasicStatic/** or **examples/VectorAddition/client/cpp/AdvancedStatic/** or **examples/VectorAddition/client/cpp/AdvancedDynamic/** directory with:

    make
    ./VectorAddition_client

### Run the Java example

Run the example from the **examples/VectorAddition/client/java/BasicStatic/** or **examples/VectorAddition/client/java/AdvancedStatic/** or **examples/VectorAddition/client/java/AdvancedDynamic/** directory with:

    ant

### Run the Ruby example

Run the example from the **examples/VectorAddition/client/rb/BasicStatic/** or **examples/VectorAddition/client/rb/AdvancedStatic/** or **examples/VectorAddition/client/rb/AdvancedDynamic/** directory with:

    ./VectorAdditionClient.rb
    
**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

    maxcompilersim -c MAIA stop
