package main

import (
    "../gen-go/com/maxeler/correlation"
    "fmt"
    "git.apache.org/thrift.git/lib/go/thrift"
    "os"
    "math/rand"
    "math"
    "strconv"
    "time"
)

const (
    correlationMaxNumVariables int64 = 6000
    correlationMaxNumTimeseries int64 = 6000
    correlationNumTopScores int64 = 10
    correlationNumPipes int64 = 12
    correlationPCIEALIGNMENT int64 = 16
    correlationNumVectorsPerBurst int64 = 2
)

// Check for errors
func checkForErrors(err error){
    if err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(-1)
    }
}

// Generate random data
func randomData(numTimeseries int64, sizeTimeseries int64) ([][]float64) {
    data := make([][]float64, numTimeseries)

    for i := int64(0); i < numTimeseries; i++ {
        data[i] = make([]float64, sizeTimeseries)
        for j := int64(0); j < sizeTimeseries; j++ {
            data[i][j] = rand.Float64()
        }
    }
    return data
}

// Calculate number of bursts for initializing LMem.
func calcNumBursts (numTimeseries int64) (int64) {
    var numVectors int64
    for i := int64(1); i <= numTimeseries; i++ {
        numVectors += (i + (correlationNumPipes - 1)) / correlationNumPipes
    }

    return (numVectors + (correlationNumVectorsPerBurst - 1)) / correlationNumVectorsPerBurst
}

// Precalculates and reorders data for the DFE.
func prepareDataForDFE (data [][]float64, sizeTimeseries int64, numTimeseries int64,
			numTimesteps int64, windowSize float64) ([]float64, []float64) {
    if (numTimeseries > correlationMaxNumTimeseries) {
        fmt.Fprintln(os.Stderr, "Number of Time series should be less or equal to %.5f. Terminating!\n",
                     correlationMaxNumTimeseries)
        os.Exit(-1)
    }
	
    if (windowSize < 2) {
        fmt.Fprintln(os.Stderr, "Window size must be equal or greater than 2. Terminating!\n")
        os.Exit(-1)
    }

    if (numTimesteps > sizeTimeseries) {
        fmt.Fprintln(os.Stderr, "Number of Time steps should be less or equal to size of Time series. Terminating!\n")
        os.Exit(-1)
    }

    var oldVal float64
    var newVal float64
    precalculations := make([]float64, 2 * numTimeseries * numTimesteps)
    dataPairs := make([]float64, 2 * numTimeseries * numTimesteps)
    sums := make([][]float64, numTimesteps)
    sumsSQ := make([][]float64, numTimesteps)
    inv := make([][]float64, numTimesteps)
    for i := int64(0); i < numTimesteps; i++ {
        sums[i] = make([]float64, numTimeseries)
        sumsSQ[i] = make([]float64, numTimeseries)
        inv[i] = make([]float64, numTimeseries)
    }

    // 2 DFE input streams: precalculations and data pairs 
    for i := int64(0); i < numTimesteps; i++ {
        for j := int64(0); j < numTimeseries; j++ {
            oldVal = 0
            if(i > int64(windowSize)) {
                oldVal = data[j][i - int64(windowSize)]
            }
            newVal = data[j][i]

            if (i == 0) {
                sums [i][j] = newVal
                sumsSQ [i][j] = newVal * newVal
            } else {
                sums[i][j] = sums[i-1][j] + newVal - oldVal
                sumsSQ[i][j] = sumsSQ[i-1][j] + newVal * newVal - oldVal * oldVal
            }

            inv[i][j] = 1 / math.Sqrt(windowSize * sumsSQ[i][j] - sums[i][j] * sums[i][j])

            // Precalculations REORDERED in DFE ORDER

            precalculations[2 * i * numTimeseries + 2 * j] = sums[i][j]
            precalculations[2 * i * numTimeseries + 2 * j + 1] = inv[i][j]

            //Data pairs REORDERED in DFE ORDER
            dataPairs[2 * i * numTimeseries + 2 * j] = newVal
            dataPairs[2 * i * numTimeseries + 2 * j + 1] = oldVal
        }
    }

    return precalculations, dataPairs
}

// Calculates number  of correlations.
func calcNumCorrelations(numTimeseries int64) (int64){
    return (numTimeseries * (numTimeseries - 1)) / 2
}

// Calculates index of correlation between i and j
func calcIndex (i int32, j int32) (int32){
    if (i == j) {
        fmt.Printf("i and j must not be the same!")
        return -1
    }
    if (i < j) {
        var tmp int32
        tmp = j
        j = i
        i = tmp
    }

    return (i * (i - 1)) / 2 + j
}

// Calculates time passed from startTime in seconds
func calcTime(startTime time.Time) (float64){
    return float64(time.Now().UnixNano() - startTime.UnixNano()) / 1000000000
}

// CorrelateDFE calculates correlations on DFE.
func CorrelateDFE (data [][]float64, sizeTimeseries int64, numTimeseries int64) ([]float64) {
    startTime := time.Now()

    // Make socket
    transport, err := thrift.NewTSocket("localhost:9090"); checkForErrors(err)

    // Wrap in a protocol
    protocol := thrift.NewTBinaryProtocolFactoryDefault()

    // Create a client to use the protocol encoder
    client := correlation.NewCorrelationServiceClientFactory(transport, protocol)

    currentTime := calcTime(startTime)
    fmt.Printf("Creating a client:\t\t\t\t%.5f\n", currentTime)

    // Connect!
    startTime = time.Now()
    err = transport.Open(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Opening connection:\t\t\t\t%.5f\n", currentTime)

    // Initialize maxfile
    startTime = time.Now()
    maxfile, err := client.CorrelationInit(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Initializing maxfile:\t\t\t\t%.5f\n", currentTime)

    // Load DFE
    startTime = time.Now()
    engine, err := client.MaxLoad(maxfile, "*"); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Loading DFE:\t\t\t\t\t%.5f\n", currentTime)

    numTimesteps := sizeTimeseries
    windowSize := float64(sizeTimeseries)
    numBursts := calcNumBursts(numTimeseries)

    // Get loop length
    startTime = time.Now()
    loopLength := make([]int32, 1)
    loopLength[0], err = client.CorrelationGet_CorrelationKernelLoopLength(); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Getting Correlation Kernel loopLength:\t\t%.5f\n", currentTime)
    
    // Prepare data for DFE 
    startTime = time.Now()

    burstSize := int64(384 / 2)  // for anything other than ISCA this should be 384
    inMemLoad := make([]int32, numBursts*burstSize)
    for i := int32(0); int64(i) < numBursts*burstSize; i++ {
        inMemLoad[i] = 0;
    }

    precalculations, dataPairs := prepareDataForDFE(data, sizeTimeseries, numTimeseries,
                                                    numTimesteps, windowSize)

    currentTime = calcTime(startTime)
    fmt.Printf("Data reordering time:\t\t\t\t%.5f\n", currentTime)

    // Allocate and send input streams to server
    startTime = time.Now()
    loopLengthSize := int64(1)
    addressLoopLength, err := client.MallocInt32T(loopLengthSize); checkForErrors(err)
    err = client.SendDataInt32T(addressLoopLength, loopLength); checkForErrors(err)
    loopLengthTime := calcTime(startTime)
    fmt.Printf("\tSending LoopLength:\t\t(size = %d bit)\t\t%.5fs\n", loopLengthSize * 32, loopLengthTime)

    startTime = time.Now()
    inMemLoadSize := numBursts * burstSize
    addressInMemLoad, err := client.MallocInt32T(inMemLoadSize); checkForErrors(err)
    err = client.SendDataInt32T(addressInMemLoad, inMemLoad); checkForErrors(err)
    inMemLoadTime := calcTime(startTime)
    fmt.Printf("\tSending InMemLoad:\t\t(size = %d bit)\t%.5fs\n", inMemLoadSize * 32, inMemLoadTime)

    startTime = time.Now()
    precalculationsSize := 2 * numTimeseries * numTimesteps
    addressPrecalculations, err := client.MallocDouble(precalculationsSize); checkForErrors(err)
    err = client.SendDataDouble(addressPrecalculations, precalculations); checkForErrors(err)
    precalculationsTime := calcTime(startTime)
    fmt.Printf("\tSending Precalculations:\t(size = %d bit)\t%.5fs\n", precalculationsSize * 64, precalculationsTime)

    startTime = time.Now()
    dataPairsSize := 2 * numTimeseries * numTimesteps
    addressDataPairs, err := client.MallocDouble(dataPairsSize); checkForErrors(err)
    err = client.SendDataDouble(addressDataPairs, dataPairs); checkForErrors(err)
    dataPairsTime := calcTime(startTime)
    fmt.Printf("\tSending DataPairs:\t\t(size = %d bit)\t%.5fs\n", dataPairsSize * 64, dataPairsTime)

    sendingTime := loopLengthTime + inMemLoadTime + precalculationsTime + dataPairsTime
    speed := float64(loopLengthSize * 32 + inMemLoadSize * 32 +
                     precalculationsSize * 64 + dataPairsSize * 64) / sendingTime / 1000000

    fmt.Printf("Sending input streams to server total time:\t%.5fs\t(average speed = %.5fMb/s)\n", sendingTime, speed)

    // Allocate memory for output stream on server
    startTime = time.Now()
    addressOutCorrelation, err := client.MallocDouble(numTimesteps * int64(loopLength[0]) *
                                                      correlationNumTopScores * correlationNumPipes +
                                                      numBursts * 24)  // for anything other than ISCA 48 should be instead of 24
    checkForErrors(err)
    addressOutIndices, err := client.MallocInt32T(2 * numTimesteps * int64(loopLength[0]) *
                                                  correlationNumTopScores * correlationNumPipes); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Allocating memory for output stream on server:\t%.5f\n", currentTime)

    // Initialize LMem
    startTime = time.Now()

    actions, err := client.MaxActionsInit(maxfile, "loadLMem"); checkForErrors(err)
    err = client.MaxSetParamUint64t(actions, "numBursts", numBursts); checkForErrors(err)
    _, err = client.MaxGetOffsetAutoLoopSize(actions, "CorrelationKernel", "loopLength"); checkForErrors(err)
    err = client.MaxQueueInput(actions, "in_memLoad", addressInMemLoad, numBursts * burstSize); checkForErrors(err)
    err = client.MaxIgnoreScalar(actions, "LMemCommandsKernel", "run_cycle_count" ); checkForErrors(err)
    err = client.MaxIgnoreScalar(actions, "CorrelationKernel", "run_cycle_count" ); checkForErrors(err)

    err = client.MaxRun(engine, actions); checkForErrors(err)

    currentTime = calcTime(startTime)
    fmt.Printf("LMem initialization:\t\t\t\t%.5f\n", currentTime)

    //Executing correlation action
    startTime = time.Now()

    actions, err = client.MaxActionsInit(maxfile, "default"); checkForErrors(err)
    err = client.MaxSetParamUint64t(actions, "numBursts", numBursts); checkForErrors(err)
    err = client.MaxSetParamUint64t(actions, "numSteps", numTimesteps); checkForErrors(err)
    err = client.MaxSetParamUint64t(actions, "numVariables", numTimeseries); checkForErrors(err)
    err = client.MaxSetParamUint64t(actions, "outputLastStep", 1); checkForErrors(err)
    err = client.MaxSetParamDouble(actions, "windowSize", windowSize); checkForErrors(err)
    err = client.MaxQueueInput(actions, "in_precalculations", addressPrecalculations,
                               2 * numTimeseries * numTimesteps * 8); checkForErrors(err)
    err = client.MaxQueueInput(actions, "in_variable_pair", addressDataPairs,
                               2 * numTimeseries * numTimesteps * 8); checkForErrors(err)
    err = client.MaxQueueOutput(actions, "out_correlation", addressOutCorrelation,
                                (numTimesteps * int64(loopLength[0]) * correlationNumTopScores * correlationNumPipes +
                                numBursts * 24) * 8); checkForErrors(err)  // for anything other than ISCA 48 should be instead of 24
    err = client.MaxQueueOutput(actions, "out_indices", addressOutIndices,
                                2 * numTimesteps * int64(loopLength[0]) *
                                correlationNumTopScores * correlationNumPipes * 4); checkForErrors(err)

    err = client.MaxRun(engine, actions); checkForErrors(err)

    currentTime = calcTime(startTime)
    fmt.Printf("Correlation time:\t\t\t\t%.5f\n", currentTime)

    // Unload DFE
    startTime = time.Now()
    err = client.MaxUnload(engine); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Unloading DFE:\t\t\t\t\t%.5f\n", currentTime)
    
    // Get output stream from server
    startTime = time.Now()
    outCorrelationSize := numTimesteps * int64(loopLength[0]) * correlationNumTopScores *
                         correlationNumPipes + numBursts * 24  // for anything other than ISCA 48 should be instead of 24
    outCorrelation, err :=client.ReceiveDataDouble(addressOutCorrelation, outCorrelationSize)
    checkForErrors(err)
    outCorrelationTime := calcTime(startTime)
    fmt.Printf("\tGet output stream Correlation:\t(size = %d bit)\t%.5f\n", outCorrelationSize * 64, outCorrelationTime)

    startTime = time.Now()
    outIndicesSize := 2 * numTimesteps * int64(loopLength[0]) * correlationNumTopScores * correlationNumPipes
    _, err = client.ReceiveDataInt32T(addressOutIndices, outIndicesSize); checkForErrors(err)
    outIndicesTime := calcTime(startTime)
    fmt.Printf("\tGet output stream outIndices:\t(size = %d bit)\t%.5f\n", outIndicesSize * 32, outIndicesTime)

    startTime = time.Now()
    loopLengthSize = int64(1)
    _, err = client.ReceiveDataInt32T(addressLoopLength, loopLengthSize); checkForErrors(err)
    loopLengthTime = calcTime(startTime)
    fmt.Printf("\tGet output stream loopLength:\t(size = %d bit)\t\t%.5f\n", loopLengthSize * 32, loopLengthTime)

    sendingTime = outCorrelationTime + outIndicesTime + loopLengthTime
    speed = float64(outCorrelationSize * 64 + outIndicesSize * 32 + loopLengthSize * 32) / sendingTime / 1000000

    fmt.Printf("Getting output stream from server total time:\t%.5fs\t(average speed = %.5fMb/s)\n", sendingTime, speed)

    // Free allocated memory for streams on server
    startTime = time.Now()
    err = client.Free(addressLoopLength); checkForErrors(err)
    err = client.Free(addressInMemLoad); checkForErrors(err)
    err = client.Free(addressPrecalculations); checkForErrors(err)
    err = client.Free(addressDataPairs); checkForErrors(err)
    err = client.Free(addressOutCorrelation); checkForErrors(err)
    err = client.Free(addressOutIndices); checkForErrors(err)
    err = client.Free(actions); checkForErrors(err)
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated memory for streams on server:\t%.5f\n", currentTime)

    // Free allocated maxfile data
    startTime = time.Now()
    client.CorrelationFree();
    currentTime = calcTime(startTime)
    fmt.Printf("Freeing allocated maxfile data:\t\t\t%.5f\n", currentTime)

    // Close!
    startTime = time.Now()
    defer transport.Close()
    currentTime = calcTime(startTime)
    fmt.Printf("Closing connection:\t\t\t\t%.5f\n", currentTime)

    // Store data
    startTime = time.Now()

    position := int64(0);
    index := int64(0);
    start := int64((numTimesteps-1) * int64(loopLength[0]) * correlationNumTopScores * correlationNumPipes);

    correlations := make([]float64, calcNumCorrelations(numTimeseries))
    for i := int64(0); i < numTimeseries; i++ {
        for j := int64(0); j < i; j++ {
            correlations[index + j] = outCorrelation[start + position + j]
        }
        index += i
        position += ((i / 12) + 1) * 12
    }

    currentTime = calcTime(startTime)
    fmt.Printf("Storing time:\t\t\t\t\t%.5f\n", currentTime)

    return correlations
}

// CorrelateCPU calculates correlations on CPU.
func CorrelateCPU (data [][]float64, numTimeseries int64, windowSize float64, numTimesteps int64) ([]float64, []int32) {
    sums := make([]float64, numTimeseries)
    sumsSQ := make([]float64, numTimeseries)
    sumsXY := make([]float64, calcNumCorrelations(numTimeseries))
    numOfCorrelations := calcNumCorrelations(numTimeseries)
    correlationsCPU := make([]float64, numOfCorrelations)
    indicesStep := make([]int32, 2 * numOfCorrelations)
    for i := int64(0); i < numTimeseries; i++ {
        sums[i] = 0
        sumsSQ[i] = 0
    }
    for i := int64(0); i < calcNumCorrelations(numTimeseries); i++ {
        sumsXY[i] = 0
    }

    for k := int64(0); k < numTimesteps; k++ {
        indexCorrelation := int64(0)
        for i := int64(0); i < numTimeseries; i++ {
            oldVal := float64(0)
            if(k  >= int64(windowSize)) {
                oldVal = data[i][k - int64(windowSize)]
            }
            newVal := data[i][k]
            sums[i] += newVal - oldVal
            sumsSQ[i] += newVal * newVal - oldVal * oldVal
        }
        for i := int32(0); int64(i) < numTimeseries; i++ {
            oldX := float64(0)
            if(k >= int64(windowSize)) {
                oldX = data[i][k - int64(windowSize)]
            }
            newX := data[i][k]
            for j := int32(0); j < i; j++ {
                oldY := float64(0)
                if(k >= int64(windowSize)) {
                    oldY = data[j][k - int64(windowSize)]
                }
                newY := data[j][k]
                sumsXY[indexCorrelation] += newX * newY - oldX * oldY
                numerator := (windowSize * sumsXY[indexCorrelation] - sums[i] * sums[j]);
                denominator := ((1 / math.Sqrt(windowSize * sumsSQ[i] - sums[i] * sums[i])) * 
                                (1 / math.Sqrt(windowSize * sumsSQ[j] - sums[j] * sums[j])))
                correlationsCPU[indexCorrelation] = numerator * denominator
                indicesStep[2 * indexCorrelation] = j
                indicesStep[2 * indexCorrelation + 1] = i
                indexCorrelation++
            }
        }
    }
    return correlationsCPU, indicesStep
}

// Checks if correlationsDFE and correlationsCPU are the same.
func check(correlationsDFE []float64, correlationsCPU []float64, numTimeseries int32, indicesStep []int32) {
    failed := int64(0);

    for i := int32(0); i < numTimeseries * (numTimeseries - 1) / 2; i++ {
        j := calcIndex(indicesStep[2 * i], indicesStep[2 * i + 1]);
        if(correlationsDFE[j] != correlationsCPU[i]) {
            failed++;
            fmt.Fprintln(os.Stderr, "correlationCPU[%d]\t=  %.20lf\n", i, correlationsCPU[i]);
            fmt.Fprintln(os.Stderr, "correlationDFE[%d]\t=  %.20lf\n", j, correlationsDFE[j]);
        }
    }
    if(failed == 0) {
        fmt.Printf("Test passed!\n");
    } else {
        fmt.Printf("Test failed %.5f times.\n", failed);
        os.Exit(-1);
    }
}

func main() {
    if(len(os.Args) != 3) {
        fmt.Printf("Usage: CorrelationClient <stream size> <number of streams>\n")
        os.Exit(-1);
    }
    sizeTimeseries, err := strconv.Atoi(os.Args[1]); checkForErrors(err)
    numTimeseries, err := strconv.Atoi(os.Args[2]); checkForErrors(err)

    data := randomData(int64(numTimeseries), int64(sizeTimeseries))

    startTime := time.Now()
    correlationsDFE := CorrelateDFE(data, int64(sizeTimeseries), int64(numTimeseries))
    currentTime := calcTime(startTime)
    fmt.Printf("DFE correlation total time:\t%.5f\n", currentTime)

    startTime = time.Now()
    correlationsCPU, indicesStep := CorrelateCPU(data, int64(numTimeseries), float64(sizeTimeseries), int64(sizeTimeseries))
    currentTime = calcTime(startTime)
    fmt.Printf("CPU correlation total time:\t%.5f\n", currentTime)

    check(correlationsDFE, correlationsCPU, int32(numTimeseries), indicesStep);   
}

