package main

import (
    "../gen-go/com/maxeler/VectorAddition"
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

func calcTime(startTime time.Time) (float64){
    return float64(time.Now().UnixNano() - startTime.UnixNano()) / 1000000000
}

// VectorAdditionCPU  implementation
func VectorAdditionCPU(x []int32, y []int32, scalar int64, size int64) ([]int32) {
    dataOut := make([]int32, size)
    scalarInt := int32(scalar)

    for i := int64(0) ; i < size ; i++ {
        dataOut[i] = x[i] + y[i] + scalarInt
    }

    return dataOut
}

// VectorAdditionDFE Advanced Static implementation
func VectorAdditionDFE(x []int32, y []int32, scalar int64, size int64) ([]int32) {
     startTime := time.Now()

     // Make socket
     transport, err := thrift.NewTSocket("localhost:9090"); checkForErrors(err)

     // Wrap in a protocol	
     protocol := thrift.NewTBinaryProtocolFactoryDefault()

     // Create a client to use the protocol encoder
     client := VectorAddition.NewVectorAdditionServiceClientFactory(transport, protocol)

     currentTime := calcTime(startTime)
     fmt.Printf("Creating a client:\t\t\t\t%.5fs\n", currentTime)

     // Connect!
     startTime = time.Now()
     err = transport.Open(); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Opening connection:\t\t\t\t%.5fs\n", currentTime)

     // Initialize maxfile
     startTime = time.Now()
     maxfile, err := client.VectorAdditionInit(); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Initializing maxfile:\t\t\t\t%.5fs\n", currentTime)

     // Load DFE
     startTime = time.Now()
     engine, err := client.MaxLoad(maxfile, "*"); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Loading DFE:\t\t\t\t\t%.5fs\n", currentTime)

     // Allocate and send input streams to server
     startTime = time.Now()
     addressX, err := client.MallocInt32T(size); checkForErrors(err)
     err = client.SendDataInt32T(addressX, x); checkForErrors(err)

     addressY, err := client.MallocInt32T(size); checkForErrors(err)
     err = client.SendDataInt32T(addressY, y); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Sending input data:\t\t\t\t%.5fs\n", currentTime)

     // Allocate memory for output stream on server
     startTime = time.Now()
     addressS, err := client.MallocInt32T(size); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Allocating memory for output stream on server:\t%.5fs\n", currentTime)

     // Writing to LMem
     startTime = time.Now()
     actionsLmem := VectorAddition.NewVectorAdditionWriteLMemActionsTStruct()
     actionsLmem.ParamAddress = 0
     actionsLmem.ParamNbytes = size * 4
     actionsLmem.InstreamCpuToLmem = addressX
     addressActionsLMem, err := client.Send_VectorAdditionWriteLMemActionsT(actionsLmem); checkForErrors(err)

     err = client.VectorAdditionWriteLMemRun(engine, addressActionsLMem); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Writing to LMem:\t\t\t\t%.5fs\n", currentTime)

     // Action default
     startTime = time.Now()
     action := VectorAddition.NewVectorAdditionActionsTStruct()
     action.Param_A = scalar
     action.Param_N = size
     action.InstreamY = addressY
     action.OutstreamS = addressS

     addressAction, err := client.Send_VectorAdditionActionsT(action); checkForErrors(err)
     err = client.VectorAdditionRun(engine, addressAction); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Vector addition time:\t\t\t\t%.5fs\n", currentTime)

     // Unload DFE
     startTime = time.Now()
     err = client.MaxUnload(engine); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Unloading DFE:\t\t\t\t\t%.5fs\n", currentTime)

     // Get output stream from server
     startTime = time.Now()
     s, err := client.ReceiveDataInt32T(addressS, size); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Getting output stream:\t(size = %v bit)\t%.5fs\n", size * 32, currentTime)

     // Free allocated memory for streams on server
     startTime = time.Now()
     client.Free(addressX); checkForErrors(err)
     client.Free(addressY); checkForErrors(err)
     client.Free(addressS); checkForErrors(err)
     currentTime = calcTime(startTime)
     fmt.Printf("Freeing allocated memory for streams on server:\t%.5fs\n", currentTime)

     // Free allocated maxfile data
     startTime = time.Now()
     client.VectorAdditionFree()
     currentTime = calcTime(startTime)
     fmt.Printf("Freeing allocated maxfile data:\t\t\t%.5fs\n", currentTime)

     // Close!
     startTime = time.Now()
     defer transport.Close()
     currentTime = calcTime(startTime)
     fmt.Printf("Closing connection:\t\t\t\t%.5fs\n", currentTime)

     return s
}
func main() {

     // Generating input data
     startTime := time.Now()
     const size int64 = 384

     x := make([]int32, size)
     y := make([]int32, size)
     scalar := int64(3)

     for i := int64(0); i < size; i++ {
         x[i] = int32(rand.Intn(100))
         y[i] = int32(rand.Intn(100))
     }
     currentTime := calcTime(startTime)
     fmt.Printf("Generating input data:\t\t\t\t%.5fs\n", currentTime)

     // DFE Output
     startTime = time.Now()
     dataOutDFE := VectorAdditionDFE(x, y, scalar, size);
     currentTime = calcTime(startTime)
     fmt.Printf("DFE vector addition total time:\t\t\t%.5fs\n", currentTime)

     // CPU Output
     startTime = time.Now()
     dataOutCPU := VectorAdditionCPU(x, y, scalar, size);
     currentTime = calcTime(startTime)
     fmt.Printf("CPU vector addition total time:\t\t\t%.5fs\n", currentTime)

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

