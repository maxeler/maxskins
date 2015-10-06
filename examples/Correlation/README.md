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

Run the example from the **examples/Correlation/client/py/BasicStatic/** directory with:

```bash
./correlation.py
```
    
**Note:** Examples can not be run if the server is not started. 

### Stopping the simulator

After executing application, simulator can be stopped with the following command::

```bash
maxcompilersim -c ISCA stop
```

