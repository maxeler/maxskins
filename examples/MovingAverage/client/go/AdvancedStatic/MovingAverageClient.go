package main

import (
    "../gen-go/com/maxeler/MovingAverage"
    "fmt"
    "git.apache.org/thrift.git/lib/go/thrift"
    "os"
    "math/rand"
)

func checkForErrors(err error){
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(-1)
    }
}

func check(dataIn []float64, dataOut []float64, size int32) {
    for i := 1; int32(i) < size - 1; i++ {
        a := float32(dataIn[i - 1])
        b := float32(dataIn[i])
        c := float32(dataIn[i + 1])
        res := (a + b + c) / 3
        if dataOut[i] != float64(res) {
            fmt.Printf("Test failed! [%v] %v != %v\n", i, dataOut[i], res)
            os.Exit(-1)
        }
    }

    fmt.Println("Test successful!")
}

func main() {
    transport, err := thrift.NewTSocket("localhost:9090"); checkForErrors(err)
    protocol := thrift.NewTBinaryProtocolFactoryDefault()
    client := MovingAverage.NewMovingAverageServiceClientFactory(transport, protocol)

    err = transport.Open(); checkForErrors(err)

    const size int32 = 384

    // Generate input data
    dataIn := make([]float64, size)
    for i := 0; int32(i) < size; i++ {
        dataIn[i] = float64(rand.Intn(100))
    }

    // Initialize maxfile
    maxfile, err := client.MovingAverageInit(); checkForErrors(err)

    // Load DFE
    engine, err := client.MaxLoad(maxfile, "*"); checkForErrors(err)

    // Allocate and send input streams to server
    address_dataIn, err := client.MallocFloat(int64(size)); checkForErrors(err)
    err = client.SendDataFloat(address_dataIn, dataIn); checkForErrors(err)

    // Allocate memory for output stream on server
    address_dataOut, err := client.MallocFloat(int64(size)); checkForErrors(err)

    // Action default
    fmt.Println("Running DFE")
    action := MovingAverage.NewMovingAverageActionsTStruct()
    action.Param_N = size
    action.InstreamX = address_dataIn
    action.OutstreamY = address_dataOut

    address_action, err := client.Send_MovingAverageActionsT(action); checkForErrors(err)
    err = client.MovingAverageRun(engine, address_action); checkForErrors(err)

    // Unload DFE
    err = client.MaxUnload(engine); checkForErrors(err)

    // Get output stream from server
    dataOut, err := client.ReceiveDataFloat(address_dataOut, int64(size)); checkForErrors(err)

    // Free allocated memory for streams on server
    err = client.Free(address_dataIn); checkForErrors(err)
    err = client.Free(address_dataOut); checkForErrors(err)
    err = client.Free(address_action); checkForErrors(err)

    // Free allocated maxfile data
    client.MovingAverageFree();

    // Checking results
    check(dataIn, dataOut, size)

    defer transport.Close()
}
