package main

import (
    "../gen-go/com/maxeler/MovingAverage"
    "fmt"
    "git.apache.org/thrift.git/lib/go/thrift"
    "os"
    "math/rand"
    "time"
)

func checkForErrors(err error){
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(-1)
    }
}

func check(dataIn []float64, dataOut []float64, size int32) (int){
    status := 0
    for i := 1; int32(i) < size - 1; i++ {
        a := float32(dataIn[i - 1])
        b := float32(dataIn[i])
        c := float32(dataIn[i + 1])
        res := (a + b + c) / 3
        if dataOut[i] != float64(res) {
            fmt.Printf("Output data @ %v = %v (expected %v)\n", i, dataOut[i], res)
            status++
        }
    }
    
    return status
}

// Calculates time passed from startTime in seconds
func calcTime(startTime time.Time) (float64){
    return float64(time.Now().UnixNano() - startTime.UnixNano()) / 1000000000
}

func main() {
    startTime := time.Now()
    startDFETime := startTime

    // Make socket
    transport, err := thrift.NewTSocket("localhost:9090"); checkForErrors(err)

    // Wrap in a protocol
    protocol := thrift.NewTBinaryProtocolFactoryDefault()
    
    // Create a client to use the protocol encoder
    client := MovingAverage.NewMovingAverageServiceClientFactory(transport, protocol)

    currentTime := calcTime(startTime)
    fmt.Printf("Creating a client:\t\t\t\t%.5fs\n", currentTime)

    // Connect!
    startTime = time.Now()
    err = transport.Open(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Opening connection:\t\t\t\t%.5fs\n", currentTime)

    startTime = time.Now()
    const size int32 = 384
    sizeBytes := size * 4
    // Generate input data
    dataIn := make([]float64, size)
    for i := 0; int32(i) < size; i++ {
        dataIn[i] = float64(rand.Intn(100))
    }
    currentTime = calcTime(startTime)
    fmt.Printf("Generating input data:\t\t\t\t%.5fs\n", currentTime)

    // Initialize maxfile
    startTime = time.Now()
    maxfile, err := client.MovingAverageInit(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Initializing maxfile:\t\t\t\t%.5fs\n", currentTime)

    // Load DFE
    startTime = time.Now()
    engine, err := client.MaxLoad(maxfile, "*"); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Loading DFE:\t\t\t\t\t%.5fs\n", currentTime)

    // Allocate and send input streams to server
    startTime = time.Now()
    addressDataIn, err := client.MallocFloat(int64(size)); checkForErrors(err)
    err = client.SendDataFloat(addressDataIn, dataIn); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Sending input data:\t\t\t\t%.5fs\n", currentTime)

    // Allocate memory for output stream on server
    startTime = time.Now()
    addressDataOut, err := client.MallocFloat(int64(size)); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Allocating memory for output stream on server:\t%.5fs\n", currentTime)

    // Action default
    startTime = time.Now()
    actions, err := client.MaxActionsInit(maxfile, "default"); checkForErrors(err)
    err = client.MaxSetParamUint64t(actions, "N", int64(size)); checkForErrors(err)
    err = client.MaxQueueInput(actions, "x", addressDataIn, int64(sizeBytes)); checkForErrors(err)
    err = client.MaxQueueOutput(actions, "y", addressDataOut, int64(sizeBytes)); checkForErrors(err)
    err = client.MaxRun(engine, actions); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Moving average time:\t\t\t\t%.5fs\n", currentTime)

    // Unload DFE
    startTime = time.Now()
    err = client.MaxUnload(engine); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Unloading DFE:\t\t\t\t\t%.5fs\n", currentTime)

    // Get output stream from server
    startTime = time.Now()
    dataOut, err := client.ReceiveDataFloat(addressDataOut, int64(size)); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Getting output stream:\t(size = %v bit)\t%.5fs\n", size * 32, currentTime)

    // Free allocated memory for streams on server
    startTime = time.Now()
    err = client.Free(addressDataIn); checkForErrors(err)
    err = client.Free(addressDataOut); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated memory for streams on server:\t%.5fs\n", currentTime)

    // Free allocated maxfile data
    startTime = time.Now()
    client.MovingAverageFree();
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated maxfile data:\t\t\t%.5fs\n", currentTime)

    // Checking results
    startTime = time.Now()
    status := check(dataIn, dataOut, size)
    currentTime = calcTime(startTime)
    fmt.Printf("Checking results:\t\t\t\t%.5fs\n", currentTime)

    startTime = time.Now()
    defer transport.Close()
    currentTime = calcTime(startTime)
    fmt.Printf("Closing connection:\t\t\t\t%.5fs\n", currentTime)

    currentTime = calcTime(startDFETime)
    fmt.Printf("DFE moving average total time:\t\t\t%.5fs\n", currentTime)

    if (status==0) { 
        fmt.Printf("Test successful!\n")
    } else {
        fmt.Printf("Test failed %v times!", status)
        os.Exit(-1)
    }
}
