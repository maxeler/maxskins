package main

import (
    "../gen-go/com/maxeler/LMemLoopback"
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

func check(dataOutDFE []int32, dataOutCPU []int32, size int64) (int64) {
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

// LMemLoopbackCPU implementation
func LMemLoopbackCPU(size int64, inA []int32, inB []int32) ([]int32) {
    dataOut := make([]int32, size)

    for i := int64(0) ; i < size ; i++ {
        dataOut[i] = inA[i] + inB[i]
    }

    return dataOut
}

// LMemLoopbackDFE AdvancedStatic implementation
func LMemLoopbackDFE(size int64, inA []int32, inB []int32) ([]int32){
    outData := make([]int32, size)
    sizeBytes := size * 4

    startTime := time.Now()
  
    // Make socket
    transport, err := thrift.NewTSocket("localhost:9090"); checkForErrors(err)

    // Wrap in a protocol	
    protocol := thrift.NewTBinaryProtocolFactoryDefault()

    // Create a client to use the protocol encoder
    client := LMemLoopback.NewLMemLoopbackServiceClientFactory(transport, protocol)

    currentTime := calcTime(startTime)
    fmt.Printf("Creating a client:\t\t\t\t%.5fs\n", currentTime)

    // Connect!
    startTime = time.Now()
    err = transport.Open(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Opening connection:\t\t\t\t%.5fs\n", currentTime)

    // Allocate and send input streams to server
    startTime = time.Now()
    addressInA, err := client.MallocInt32T(size); checkForErrors(err)
    client.SendDataInt32T(addressInA, inA)

    addressInB, err := client.MallocInt32T(size); checkForErrors(err)
    client.SendDataInt32T(addressInB, inB);
    currentTime = calcTime(startTime)
    fmt.Printf("Sending input data:\t\t\t\t%.5fs\n", currentTime)

    // Initialize maxfile
    startTime = time.Now()
    maxfile, err := client.LMemLoopbackInit(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Initializing maxfile:\t\t\t\t%.5fs\n", currentTime)

    // Load DFE
    startTime = time.Now()
    engine, err := client.MaxLoad(maxfile, "*"); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Loading DFE:\t\t\t\t\t%.5fs\n", currentTime)

    // Allocate memory for output stream on server
    startTime = time.Now()
    addressOutData, err := client.MallocInt32T(size); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Allocating memory for output stream on server:\t%.5fs\n", currentTime)

    // Write to LMem
    startTime = time.Now()
    actionsLmemWrite := LMemLoopback.NewLMemLoopbackWriteLMemActionsTStruct()

    actionsLmemWrite.ParamAddress = 0
    actionsLmemWrite.ParamNbytes = sizeBytes
    actionsLmemWrite.InstreamCpuToLmem = addressInA

    addressActionsLmemWrite, err := client.Send_LMemLoopbackWriteLMemActionsT(actionsLmemWrite); checkForErrors(err)
    err = client.LMemLoopbackWriteLMemRun(engine, addressActionsLmemWrite); checkForErrors(err)

    actionsLmemWrite.ParamAddress = sizeBytes
    actionsLmemWrite.ParamNbytes = sizeBytes
    actionsLmemWrite.InstreamCpuToLmem = addressInB

    addressActionsLmemWrite, err = client.Send_LMemLoopbackWriteLMemActionsT(actionsLmemWrite); checkForErrors(err)
    err = client.LMemLoopbackWriteLMemRun(engine, addressActionsLmemWrite); checkForErrors(err)
    client.Free(addressActionsLmemWrite);
    currentTime = calcTime(startTime)
    fmt.Printf("Writing to LMem:\t\t\t\t%.5fs\n", currentTime)

    // Action default
    startTime = time.Now()
    actions := LMemLoopback.NewLMemLoopbackActionsTStruct();

    actions.Param_N = int32(size)

    addressActions, err := client.Send_LMemLoopbackActionsT(actions); checkForErrors(err)
    err = client.LMemLoopbackRun(engine, addressActions); checkForErrors(err)
    client.Free(addressActions)
    currentTime = calcTime(startTime)
    fmt.Printf("LMemLoopback time:\t\t\t\t%.5fs\n", currentTime)

    // Read from LMem
    startTime = time.Now()
    actionsLmemRead := LMemLoopback.NewLMemLoopbackReadLMemActionsTStruct()

    actionsLmemRead.ParamAddress = 2 * sizeBytes
    actionsLmemRead.ParamNbytes = sizeBytes
    actionsLmemRead.OutstreamLmemToCpu = addressOutData

    addressActionsLmemRead, err := client.Send_LMemLoopbackReadLMemActionsT(actionsLmemRead); checkForErrors(err)
    err = client.LMemLoopbackReadLMemRun(engine, addressActionsLmemRead); checkForErrors(err)
    client.Free(addressActionsLmemRead)
    currentTime = calcTime(startTime)
    fmt.Printf("Reading from LMem:\t\t\t\t%.5fs\n", currentTime)

    // Unload DFE
    startTime = time.Now()
    err = client.MaxUnload(engine); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Unloading DFE:\t\t\t\t\t%.5fs\n", currentTime)

    // Get output stream from server
    startTime = time.Now()
    outData, err = client.ReceiveDataInt32T(addressOutData, size); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Getting output stream:\t(size = %v bit)\t%.5fs\n", size * 32, currentTime)

    // Free allocated memory for streams on server
    startTime = time.Now()
    client.Free(addressInA)
    client.Free(addressInB)
    client.Free(addressOutData)
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated memory for streams on server:\t%.5fs\n", currentTime)

    // Free allocated maxfile data
    startTime = time.Now()
    client.LMemLoopbackFree()
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated maxfile data:\t\t\t%.5fs\n", currentTime)

    // Close!
    startTime = time.Now()
    transport.Close()
    currentTime = calcTime(startTime)
    fmt.Printf("Closing connection:\t\t\t\t%.5fs\n", currentTime)

    return outData
}

func main() {
    // Generating input data
    startTime := time.Now()
    size := int32(384)
    inA := make([]int32, size)
    inB := make([]int32, size)

    for i := int32(0); i < size; i++ {
        inA[i] = i
        inB[i] = size - i
    }
    currentTime := calcTime(startTime)
    fmt.Printf("Generating input data:\t\t\t\t%.5fs\n", currentTime)

    // DFE Output
    startTime = time.Now()
    dataOutDFE := LMemLoopbackDFE(int64(size), inA, inB);
    currentTime = calcTime(startTime)
    fmt.Printf("DFE LMemLoopback total time:\t\t\t%.5fs\n", currentTime)

    // CPU Output
    startTime = time.Now()
    dataOutCPU := LMemLoopbackCPU(int64(size), inA, inB);
    currentTime = calcTime(startTime)
    fmt.Printf("CPU LMemLoopback total time:\t\t\t%.5fs\n", currentTime)

    // Checking results
    startTime = time.Now()
    status := check(dataOutDFE, dataOutCPU, int64(size))
    currentTime = calcTime(startTime)
    fmt.Printf("Checking results:\t\t\t\t%.5fs\n", currentTime)

    if status == 0 {
        fmt.Printf("Test passed!\r\n")
    } else {
        fmt.Printf("Test failed %v times!", status)
        os.Exit(-1)
    }
}
