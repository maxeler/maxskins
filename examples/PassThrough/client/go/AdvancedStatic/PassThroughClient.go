package main

import (
    "../gen-go/com/maxeler/PassThrough"
    "fmt"
    "git.apache.org/thrift.git/lib/go/thrift"
    "os"
    "time"
)

func checkForErrors(err error){
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(-1)
    }
}

func check(dataOutDFE []float64, dataOutCPU []float64, size int64) (int64) {
    status := int64(0);

    for i := int64(0); i < size; i++ {
        if dataOutDFE[i] != dataOutCPU[i] {
            fmt.Printf("Output data @ %v = %v (expected %v)\r\n", i, dataOutDFE[i], dataOutCPU[i])
            status++
        }
    }

    return status;
}

// Calculates time passed from startTime in seconds
func calcTime(startTime time.Time) (float64){
    return float64(time.Now().UnixNano() - startTime.UnixNano()) / 1000000000
}

// PassThroughCPU  implementation
func PassThroughCPU(size int64, dataIn []float64) ([]float64) {
    dataOut := make([]float64, size)

    for i := int64(0) ; i < size ; i++ {
        dataOut[i] = dataIn[i]
    }

    return dataOut
}

// PassThroughDFE AdvancedStatic implementation
func PassThroughDFE(size int64, dataIn []float64) ([]float64) {
    dataOut := make([]float64, size)
    startTime := time.Now()

    // Make socket
    transport, err := thrift.NewTSocket("localhost:9090"); checkForErrors(err)

    // Wrap in a protocol
    protocol := thrift.NewTBinaryProtocolFactoryDefault()
    
    // Create a client to use the protocol encoder
    client := PassThrough.NewPassThroughServiceClientFactory(transport, protocol)

    currentTime := calcTime(startTime)
    fmt.Printf("Creating a client:\t\t\t\t%.5fs\n", currentTime)

    // Connect!
    startTime = time.Now()
    err = transport.Open(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Opening connection:\t\t\t\t%.5fs\n", currentTime)
    
    // Initialize maxfile
    startTime = time.Now()
    maxfile, err := client.PassThroughInit(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Initializing maxfile:\t\t\t\t%.5fs\n", currentTime)

    // Load DFE
    startTime = time.Now()
    engine, err := client.MaxLoad(maxfile, "*"); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Loading DFE:\t\t\t\t\t%.5fs\n", currentTime)

    // Allocate and send input streams to server
    startTime = time.Now()
    addressDataIn, err := client.MallocFloat(size); checkForErrors(err)
    err = client.SendDataFloat(addressDataIn, dataIn); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Sending input data:\t\t\t\t%.5fs\n", currentTime)

    // Allocate memory for output stream on server
    startTime = time.Now()
    addressDataOut, err := client.MallocFloat(size); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Allocating memory for output stream on server:\t%.5fs\n", currentTime)

    // Action default
    startTime = time.Now()
    action := PassThrough.NewPassThroughActionsTStruct()
    action.Param_N = int32(size)
    action.InstreamX = addressDataIn
    action.OutstreamY = addressDataOut
    addressAction, err := client.Send_PassThroughActionsT(action); checkForErrors(err)
    err = client.PassThroughRun(engine, addressAction); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Pass through time:\t\t\t\t%.5fs\n", currentTime)
 
    // Unload DFE
    startTime = time.Now()
    err = client.MaxUnload(engine); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Unloading DFE:\t\t\t\t\t%.5fs\n", currentTime)

    // Get output stream from server
    startTime = time.Now()
    dataOut, err = client.ReceiveDataFloat(addressDataOut, size); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Getting output stream:\t(size = %v bit)\t%.5fs\n", size * 32, currentTime)

    // Free allocated memory for streams on server
    startTime = time.Now()
    client.Free(addressDataIn)
    client.Free(addressDataOut)
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated memory for streams on server:\t%.5fs\n", currentTime)

    // Free allocated maxfile data
    startTime = time.Now()
    client.PassThroughFree();
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated maxfile data:\t\t\t%.5fs\n", currentTime)

    startTime = time.Now()
    transport.Close()
    currentTime = calcTime(startTime)
    fmt.Printf("Closing connection:\t\t\t\t%.5fs\n", currentTime)

    return dataOut
}

func main() {
    // Input
    startTime := time.Now()
    size := int64(1024);
    dataIn := make([]float64, size);

    for i := int64(0); i < size; i++ {
        dataIn[i] = float64(i + 1);
    }
    currentTime := calcTime(startTime)
    fmt.Printf("Generating input data:\t\t\t\t%.5fs\n", currentTime)

    // DFE Output
    startTime = time.Now()
    dataOutDFE := PassThroughDFE(size, dataIn);
    currentTime = calcTime(startTime)
    fmt.Printf("DFE pass through total time:\t\t\t%.5fs\n", currentTime)
    
    // CPU Output
    startTime = time.Now()
    dataOutCPU := PassThroughCPU(size, dataIn);
    currentTime = calcTime(startTime)
    fmt.Printf("CPU pass through total time:\t\t\t%.5fs\n", currentTime)

    // Checking results
    startTime = time.Now()
    status := check(dataOutDFE, dataOutCPU, size)
    currentTime = calcTime(startTime)
    fmt.Printf("Checking results:\t\t\t\t%.5fs\n", currentTime)
    
    if status == 0 {
        fmt.Printf("Test passed!\r\n")
    } else {
        fmt.Printf("Test failed %v times!", status)
        os.Exit(-1)
    }
}
