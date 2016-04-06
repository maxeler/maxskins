package main

import (
    "../gen-go/com/maxeler/SignExt"
    "fmt"
    "git.apache.org/thrift.git/lib/go/thrift"
    "os"
    "math"
    "time"
)

func checkForErrors(err error){
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(-1)
    }
}

func main() {
    transport, err := thrift.NewTSocket("localhost:9090"); checkForErrors(err)
    protocol := thrift.NewTBinaryProtocolFactoryDefault()
    client := SignExt.NewSignExtServiceClientFactory(transport, protocol)

    err = transport.Open(); checkForErrors(err)

    if len(os.Args) != 3 {
        fmt.Println("Usage: $0 dfe_ip remote_ip")
        os.Exit(-1)
    }

    dfeIPAddress, err := client.MallocInt64T(5); checkForErrors(err)
    client.InetAton(os.Args[1], dfeIPAddress)

    remoteIPAddress, err := client.MallocInt64T(5); checkForErrors(err)
    client.InetAton(os.Args[2], remoteIPAddress)

    netmaskAddress, err := client.MallocInt64T(5); checkForErrors(err)
    client.InetAton("255.255.255.0", netmaskAddress)

    port := 2000

    // Initialize maxfile
    maxfile, err := client.SignExtInit(); checkForErrors(err)

    // Load DFE
    engine, err := client.MaxLoad(maxfile, "*"); checkForErrors(err)

    enumkey := SignExt.NewMaxConfigKeyBoolTStruct()
    enumkey.TypeA1 = SignExt.MaxConfigKeyBoolTEnum_MAX_CONFIG_PRINTF_TO_STDOUT
    err = client.MaxConfigSetBool(enumkey, 1); checkForErrors(err)

    actions, err := client.MaxActionsInit(maxfile, "default"); checkForErrors(err)

    client.MaxRun(engine, actions)
    client.MaxActionsFree(actions)

    bufferAddress, err := client.MallocInt64T(1); checkForErrors(err)
    bufferSize := int64(4096 * 512)
    client.PosixMemalign(bufferAddress, int64(4096), bufferSize)

    bufferVec, err := client.ReceiveDataInt64T(bufferAddress, 1); checkForErrors(err)
    buffer := bufferVec[0]

    toCPU, err := client.MaxFramedStreamSetup(engine, "toCPU", SignExt.RemotePtr(buffer), bufferSize, -1); checkForErrors(err)

    enumconn := SignExt.NewMaxNetConnectionTStruct()
    enumconn.TypeA1 =  SignExt.MaxNetConnectionTEnum_MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1
    client.MaxIpConfig(engine, enumconn, dfeIPAddress, netmaskAddress);
    dfeSocket, err := client.MaxUdpCreateSocket(engine, "udpTopPort1"); checkForErrors(err)
    client.MaxUdpBind(dfeSocket, int16(port))
    client.MaxUdpConnect(dfeSocket, remoteIPAddress, 0)

    fmt.Printf("Listening on %v port %v\r\n", os.Args[1], port)

    fmt.Println("Waiting for kernel response...")

    fAddress, err := client.MallocInt64T(1); checkForErrors(err)
    fszAddress, err := client.MallocInt64T(1); checkForErrors(err)
    numMessageRx := 0
    cond := 1

    for cond == 1 {
        maxFrames, err := client.MaxFramedStreamRead(toCPU, 1, fAddress, fszAddress); checkForErrors(err)
        if(maxFrames == 1) {
            numMessageRx++

            fszVec, err := client.ReceiveDataInt64T(fszAddress, 1);checkForErrors(err) 
            fsz := fszVec[0]
            fmt.Printf("CPU: Got output frame %v - size %v bytes\r\n", numMessageRx, fsz)

            fVec, err := client.ReceiveDataInt64T(fAddress, 1); checkForErrors(err)
            f := fVec[0]

            w, err := client.ReceiveDataInt64T(SignExt.RemotePtr(f), 3); checkForErrors(err)

            var wp uint64
            for i := 0; i < 3; i++ {
                if(w[i] < 0) {
                    wp = uint64(w[i] + int64(math.Pow(2, 32)) * int64(math.Pow(2, 32)))
                } else {
                    wp = uint64(w[i])
                }
                fmt.Printf("Frame [%v] Word[%v]: 0x%X\r\n", numMessageRx, i, wp)
            }
                    
            client.MaxFramedStreamDiscard(toCPU, 1)

            if(w[0] == 0 && w[1] == 0 && w[2] == 0) {
                cond = 0
            }

        } else {
            time.Sleep(1/100000 * time.Millisecond)
        }
    }

    client.MaxUdpClose(dfeSocket)
    client.MaxFramedStreamRelease(toCPU)
    client.MaxUnload(engine)
    client.MaxFileFree(maxfile)
    client.Free(dfeIPAddress)
    client.Free(remoteIPAddress)
    client.Free(netmaskAddress)
    client.Free(bufferAddress)
    client.Free(fAddress)
    client.Free(fszAddress)
    client.SignExtFree()

    fmt.Println("Done.")

    // Close!
    transport.Close()
    os.Exit(0)
}
