# Correlation example

## Overview

Statistical method to analyze the relationship between two random variables.

## Running this example

This DFE is compiled for ISCA card using MaxCompiler 2014.2.

### Starting the simulator

To run it using simulator, first simulated ISCA card needs to be (re)started:

    maxcompilersim -c ISCA restart

These environment variables need to be exported so this example can be executed:

    export MAXELEROSDIR=$MAXCOMPILERDIR/lib/maxeleros-sim
    export LD_PRELOAD=$MAXELEROSDIR/lib/libmaxeleros.so:$LD_PRELOAD
    export SLIC_CONF="$SLIC_CONF;use_simulation=sim"

### Create skin for Python

Create skin from **examples/Correlation** directory with:

    maxskins -t py correlation.max
    
**Note:** maxskins command creates **client**, **server** and **.scratch** directories in the current directory.    
In the **client** directory there are Apache Thrift files necessary for client. 
In the **server** directory there is a binary file which is used to start the server.   
in the **.scratch** directory there are all files that are generated during the compiling process.  

### Start the server

Start the server from the created server directory (**examples/Correlation/server**) with:

    ./correlation_server 9090 &

### Run the Python example

Run the example from the **examples/Correlation/client/py/BasicStatic/** directory with:

    python correlation.py
    
**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

    maxcompilersim -c ISCA stop
