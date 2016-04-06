module Main where

import qualified CorrelationService_Client as Client

import Thrift
import Thrift.Protocol.Binary
import Thrift.Server
import Thrift.Transport
import Thrift.Transport.Handle


import Control.Exception
import Data.Array.IO
import Data.Either
import Data.Int
import Data.List
import Data.Maybe
import Data.Time
import Data.Vector
import Network
import System.Environment
import System.Exit
import System.Random
import Text.Printf

-- Maximal number of variables.
correlationMaxNumVariables = 6000
-- Maximal number of Time series.
correlationMaxNumTimeseries = 6000
-- Maximal number of top scores.
correlationNumTopScores = 10
-- Number of pipes.
correlationNumPipes = 12
-- PCIe alignment.
correlationPCIE_ALIGNMENT = 16
-- Number of vectors per burst.
correlationNumVectorsPerBurst = 2

-- For anything other than ISCA this should be 384.
burstSize = 192

-- Number of nano seconds in one second.
numOfNanoSeconds = 1000000000

-- Size of double in bits.
sizeOfDouble = 64

-- Size of int in bits.
sizeOfInt = 32

-- Size of megabyte in bytes.
sizeOfMegabyte = 1000000

getRight           :: Either left right -> right
getRight (Right x) = x

check :: [Double] -> [Double] -> Int -> Int -> Bool
check corrCPU corrDFE numOfCorrelations numTimeseries = do
                                                 let indicesStep1 = [ j | i <- [0 .. (numTimeseries-1)], j <- [0 .. (i-1)] ]
                                                 let indicesStep2 = [ i | i <- [0 .. (numTimeseries-1)], j <- [0 .. (i-1)] ]
                                                 let indexes =  [  calcIndex (indicesStep1!!i) (indicesStep2!!i) | i <-  [0 .. (numOfCorrelations-1) ]]
                                                 let corrDFEordered =  [  corrDFE !! (indexes !! i) | i <-  [0 .. (numOfCorrelations-1) ]]
                                                 (corrDFEordered == corrCPU)                                                
                                                
randomData :: Int -> Int-> StdGen -> [[Double]]
randomData  1 sizeTimeseries seed                =    Data.List.take sizeTimeseries (randomRs (0, 1) seed) : []
randomData  numTimeseries sizeTimeseries seed    =    Data.List.take sizeTimeseries (randomRs (0, 1) seed) : (randomData (numTimeseries-1) sizeTimeseries (snd (next seed)))

calcNumBursts :: Int -> Int -> Int -> Int
calcNumBursts 1 numTimeseries tmpNumBrusts      =    ((calcNumBursts 2 numTimeseries ((1 + (correlationNumPipes - 1)) `div` correlationNumPipes)) + (correlationNumVectorsPerBurst-1)) `div` correlationNumVectorsPerBurst
calcNumBursts i numTimeseries tmpNumBrusts
    | i < numTimeseries     =    (calcNumBursts (i + 1) numTimeseries (tmpNumBrusts + ((i + (correlationNumPipes - 1)) `div` correlationNumPipes)))
    | i == numTimeseries    =    tmpNumBrusts + ((i + (correlationNumPipes - 1)) `div` correlationNumPipes)

realPrepareDataForDFE :: [[Double]] -> Int -> Int -> Int -> Double -> Int -> Int -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> (Int, [Double], [Double])
realPrepareDataForDFE dataIn sizeTimeseries numTimeseries numTimesteps windowSize i j sums sumsSQ inv  precalculations dataPairs
    | i == numTimesteps - 1 && j == numTimeseries - 1 =    do
        let oldVal = if i > (round windowSize) then ((dataIn !! j) !! i) else 0
    	let newVal = ((dataIn !! j) !! i)
	let sumsNew = if i == 0 then (sums Data.List.++ [newVal]) else (sums Data.List.++ [((sums !! (((i-1)*numTimeseries) + j)) + newVal - oldVal)])
	let sumsSQNew = if i == 0 then (sumsSQ Data.List.++ [(newVal * newVal)]) else (sumsSQ Data.List.++ [((sumsSQ !! (((i-1)*numTimeseries) + j)) + (newVal * newVal) - (oldVal * oldVal))])
	let invNew = (inv Data.List.++ [(1/(sqrt((windowSize *  (sumsSQNew !! ((i*numTimeseries) + j))) - ((sumsNew !! ((i*numTimeseries) + j)) * (sumsNew !! ((i*numTimeseries) + j))))))])
	let precalculationsNew = (precalculations Data.List.++ [(sumsNew !! ((i*numTimeseries) + j))] Data.List.++ [(invNew !! ((i*numTimeseries) + j))])
	let dataPairsNew = (dataPairs Data.List.++ [newVal] Data.List.++ [oldVal])
	(0, precalculationsNew, dataPairsNew)

    | j == numTimeseries - 1    =    do
        let oldVal = if i > (round windowSize) then ((dataIn !! j) !! i) else 0
    	let newVal = ((dataIn !! j) !! i)
	let sumsNew = if i == 0 then (sums Data.List.++ [newVal]) else (sums Data.List.++ [((sums !! (((i-1)*numTimeseries) + j)) + newVal - oldVal)])
	let sumsSQNew = if i == 0 then (sumsSQ Data.List.++ [(newVal * newVal)]) else (sumsSQ Data.List.++ [((sumsSQ !! (((i-1)*numTimeseries) + j)) + (newVal * newVal) - (oldVal * oldVal))])
	let invNew = (inv Data.List.++ [(1/(sqrt((windowSize *  (sumsSQNew !! ((i*numTimeseries) + j))) - ((sumsNew !! ((i*numTimeseries) + j)) * (sumsNew !! ((i*numTimeseries) + j))))))])
	let precalculationsNew = (precalculations Data.List.++ [(sumsNew !! ((i*numTimeseries) + j))] Data.List.++ [(invNew !! ((i*numTimeseries) + j))])
	let dataPairsNew = (dataPairs Data.List.++ [newVal] Data.List.++ [oldVal])
	(realPrepareDataForDFE dataIn sizeTimeseries numTimeseries numTimesteps windowSize (i+1) 0 sumsNew sumsSQNew invNew precalculationsNew dataPairsNew)

    | otherwise    =    do
        let oldVal = if i > (round windowSize) then ((dataIn !! j) !! i) else 0
    	let newVal = ((dataIn !! j) !! i)
	let sumsNew = if i == 0 then (sums Data.List.++ [newVal]) else (sums Data.List.++ [((sums !! (((i-1)*numTimeseries) + j)) + newVal - oldVal)])
	let sumsSQNew = if i == 0 then (sumsSQ Data.List.++ [(newVal * newVal)]) else (sumsSQ Data.List.++ [((sumsSQ !! (((i-1)*numTimeseries) + j)) + (newVal * newVal) - (oldVal * oldVal))])
	let invNew = (inv Data.List.++ [(1/(sqrt((windowSize *  (sumsSQNew !! ((i*numTimeseries) + j))) - ((sumsNew !! ((i*numTimeseries) + j)) * (sumsNew !! ((i*numTimeseries) + j))))))])
	let precalculationsNew = (precalculations Data.List.++ [(sumsNew !! ((i*numTimeseries) + j))] Data.List.++ [(invNew !! ((i*numTimeseries) + j))])
	let dataPairsNew = (dataPairs Data.List.++ [newVal] Data.List.++ [oldVal])
	(realPrepareDataForDFE dataIn sizeTimeseries numTimeseries numTimesteps windowSize i (j+1) sumsNew sumsSQNew invNew precalculationsNew dataPairsNew)

storeData :: [Double] -> Int -> Int -> Int -> Int -> Int -> [Double] -> [Double]
storeData outCorrelation start position index numTimeseries i correlations
    | i == numTimeseries    =    correlations
    | i < numTimeseries     =    do
        let correlationsNew = correlations Data.List.++ (Data.List.take (i) (Data.List.drop (start + position) outCorrelation))
        let indexNew = index + 1
        let positionNew = position + (((i `div` correlationNumPipes) + 1) * correlationNumPipes)
        (storeData outCorrelation start positionNew indexNew numTimeseries (i + 1) correlationsNew)

calcNumCorrelations :: Int -> Int
calcNumCorrelations numTimeseries    =    (numTimeseries * (numTimeseries - 1)) `div` 2

prepareDFE :: [[Double]] -> Int -> Int -> Int -> ([Double], [Double])
prepareDFE dataIn numTimeseries numTimesteps windowSize = do
    let rows = numTimesteps
    let columns = numTimeseries
    let nElements = numTimesteps*numTimeseries
    let newVal = [ ((dataIn!!j)!!i) | i <- [0 .. (numTimesteps-1)], j <- [0 .. (numTimeseries-1)] ]    
    let newValSquared = Data.List.map (**2) newVal
        
    let sums = [ if i == 0 then newVal!!(i*numTimeseries+j) else ( Data.List.sum (Data.List.take i [newVal!!((p*numTimeseries) + j) | p <- [0 .. (numTimesteps-1)]]) + newVal!!(i*numTimeseries+j) ) | i <- [0 .. (numTimesteps-1)], j <- [0 .. (numTimeseries-1)] ]

    let sumsSq = [ if i == 0 then newValSquared!!(i*numTimeseries+j) else ( Data.List.sum (Data.List.take i [newValSquared!!((p*numTimeseries) + j) | p <- [0 .. (numTimesteps-1)]]) + newValSquared!!(i*numTimeseries+j) ) | i <- [0 .. (numTimesteps-1)], j <- [0 .. (numTimeseries-1)] ]

    let sumsSquared = Data.List.map (**2) sums

    let inv =  [ 1 / sqrt ((fromIntegral windowSize) * (sumsSq!!i) - (sumsSquared!!i)) | i <- [0 .. (nElements-1)] ]

    let precalculationsPairs = [ [(sums!!i), (inv!!i)]  | i <- [0 .. (nElements-1)]  ] 
    let precalculations = Data.List.concat precalculationsPairs

    let dataPairsCombined = [  [(newVal!!i), 0]  | i <- [0 .. (nElements-1)]  ]  
    let dataPairs = Data.List.concat dataPairsCombined

    (precalculations, dataPairs)


calcIndex :: Int -> Int -> Int
calcIndex row column
    | row == column    =    (-1)
    | otherwise        =    do
        let bigger = if row < column then column else row
	let smaller = if row < column then row else column
	(((bigger * (bigger - 1)) `div` 2) + smaller)

correlateDFE :: [[Double]] -> Int -> Int -> IO ([Double])
correlateDFE dataIn numTimeseries sizeTimeseries    =    do
    startTime <- getCurrentTime

    -- Make socket
    transport <- hOpen ("localhost", PortNumber 9090)

    -- Wrap in a protocol
    let protocol = BinaryProtocol transport

    -- Create a client to use the protocol encoder
    let client = (protocol, protocol)

    stopTime <- getCurrentTime
    putStrLn ("Createing a client and opening connection:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    let numTimesteps = sizeTimeseries
    let windowSize = sizeTimeseries
    let numBursts = calcNumBursts 0 numTimeseries 0

    -- Get loop length
    startTime <- getCurrentTime
    e <- try (Client.correlation_get_CorrelationKernel_loopLength client) :: IO (Either SomeException Int32)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let loopLength = getRight e
    stopTime <- getCurrentTime
    putStrLn ("Geting Correlation Kernel loopLength:\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Prepare data for DFE
    startTime <- getCurrentTime
    let inMemLoad = Data.List.take (numBursts * burstSize) [0,0..]
    -- Old prepareFroDFE method
    --let (status, precalculations, dataPairs) = realPrepareDataForDFE dataIn sizeTimeseries numTimeseries sizeTimeseries (fromIntegral sizeTimeseries) 0 0 [] [] [] [] []
    -- New prepareForDFE method, runs a bit faster for larger inputs
    let (precalculations, dataPairs) = prepareDFE dataIn numTimeseries numTimesteps windowSize
    
    stopTime <- getCurrentTime
    putStrLn ("Data reordering time:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Allocate and send input streams to server
    startTime <- getCurrentTime
    let loopLengthSize = 1
    e <- try (Client.malloc_int32_t client loopLengthSize) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let addressLoopLength = getRight e
    e <- try (Client.send_data_int32_t client addressLoopLength (fromList [loopLength])) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    let loopLengthTime = diffUTCTime stopTime startTime
    putStrLn ("\tSending LoopLength:\t\t(size = " Data.List.++ (show (loopLengthSize * 32)) Data.List.++ " bit)\t\t" Data.List.++ (show loopLengthTime))

    startTime <- getCurrentTime
    let inMemLoadSize = numBursts * burstSize
    e <- try (Client.malloc_int32_t client $ fromIntegral inMemLoadSize) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let addressInMemLoad = getRight e
    e <- try (Client.send_data_int32_t client addressInMemLoad (fromList inMemLoad)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    let inMemLoadTime = diffUTCTime stopTime startTime
    putStrLn ("\tSending InMemLoad:\t\t(size = " Data.List.++ (show (inMemLoadSize * sizeOfInt)) Data.List.++ " bit)\t" Data.List.++ (show inMemLoadTime))

    startTime <- getCurrentTime
    let precalculationsSize = 2 * numTimeseries * numTimesteps
    e <- try (Client.malloc_double client $ fromIntegral precalculationsSize) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let addressPrecalculations = getRight e

    e <- try (Client.send_data_double client addressPrecalculations (fromList precalculations)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    let precalculationsTime = diffUTCTime stopTime startTime
    putStrLn ("\tSending Precalculations:\t(size = " Data.List.++ (show (precalculationsSize * sizeOfDouble)) Data.List.++ " bit)\t" Data.List.++ (show precalculationsTime))

    startTime <- getCurrentTime
    let dataPairsSize = 2 * numTimeseries * numTimesteps
    e <- try (Client.malloc_double client $ fromIntegral dataPairsSize) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let addressDataPairs = getRight e
    e <- try (Client.send_data_double client addressDataPairs (fromList dataPairs)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    let dataPairsTime = diffUTCTime stopTime startTime
    putStrLn ("\tSending DataPairs:\t\t(size = " Data.List.++ (show (dataPairsSize * sizeOfDouble)) Data.List.++ " bit)\t" Data.List.++ (show dataPairsTime))

    let estimatedTime = realToFrac (loopLengthTime + inMemLoadTime + precalculationsTime + dataPairsTime)
    let speed = (fromIntegral (loopLengthSize * (fromIntegral sizeOfInt)) + fromIntegral(inMemLoadSize * (fromIntegral sizeOfInt)) + fromIntegral(precalculationsSize * sizeOfDouble) + fromIntegral(dataPairsSize * sizeOfDouble)) / estimatedTime / sizeOfMegabyte
    putStrLn ("Sending input streams to server total time:\t" Data.List.++ (show estimatedTime) Data.List.++ "s\t(average speed = " Data.List.++ (show speed) Data.List.++ "Mb/s)")

    -- Allocate memory for output stream on server
    startTime <- getCurrentTime
    e <- try (Client.malloc_double client $ fromIntegral (numTimesteps * (fromIntegral loopLength) * correlationNumTopScores * correlationNumPipes + numBursts * 24)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let addressOutCorrelation = getRight e
    e <- try (Client.malloc_double client $ fromIntegral (2* numTimesteps * (fromIntegral loopLength) * correlationNumTopScores * correlationNumPipes)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let addressOutIndices = getRight e
    stopTime <- getCurrentTime
    putStrLn ("Allocating memory for output stream on server:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Initialize LMem
    startTime <- getCurrentTime
    e <- try (Client.correlation_loadLMem client (fromIntegral numBursts) addressLoopLength addressInMemLoad) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    putStrLn ("LMem initialization:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Executing correlation action
    startTime <- getCurrentTime
    e <- try (Client.correlation client (fromIntegral numBursts) (fromIntegral numTimesteps) (fromIntegral numTimeseries) 1 (fromIntegral windowSize) addressPrecalculations addressDataPairs addressOutCorrelation addressOutIndices) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    putStrLn ("Correlation time:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Get output stream from server
    startTime <- getCurrentTime
    let outCorrelationSize = numTimesteps * (fromIntegral loopLength) * correlationNumTopScores * correlationNumPipes + numBursts * 24
    e <- try (Client.receive_data_double client addressOutCorrelation (fromIntegral outCorrelationSize)) :: IO (Either SomeException (Vector Double))
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let outCorrelation = getRight e
    stopTime <- getCurrentTime
    let outCorrelationTime = diffUTCTime stopTime startTime
    putStrLn ("\tGet output stream Correlation:\t(size = " Data.List.++ (show (outCorrelationSize * sizeOfDouble)) Data.List.++ " bit)\t" Data.List.++ (show outCorrelationTime))

    startTime <- getCurrentTime
    let outIndicesSize = 2 * numTimesteps * (fromIntegral loopLength) * correlationNumTopScores * correlationNumPipes
    e <- try (Client.receive_data_double client addressOutIndices (fromIntegral outIndicesSize)) :: IO (Either SomeException (Vector Double))
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let outIndices = getRight e
    stopTime <- getCurrentTime
    let outIndicesTime = diffUTCTime stopTime startTime
    putStrLn ("\tGet output stream outIndices:\t(size = " Data.List.++ (show (outIndicesSize * sizeOfInt)) Data.List.++ " bit)\t" Data.List.++ (show outIndicesTime))

    startTime <- getCurrentTime
    let loopLengthSize = 1
    e <- try (Client.receive_data_int32_t client addressLoopLength (fromIntegral loopLengthSize)) :: IO (Either SomeException (Vector Int32))
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let loopLength = getRight e
    stopTime <- getCurrentTime
    let loopLengthOutTime = diffUTCTime stopTime startTime
    putStrLn ("\tGet output stream loopLength:\t(size = " Data.List.++ (show (loopLengthSize * sizeOfInt)) Data.List.++ " bit)\t\t" Data.List.++ (show loopLengthOutTime))

    let estimatedOutTime = realToFrac (outCorrelationTime + outIndicesTime + loopLengthOutTime)
    let speedOut = (fromIntegral (outCorrelationSize * sizeOfDouble) + fromIntegral (outIndicesSize * sizeOfInt) + fromIntegral (loopLengthSize * sizeOfInt)) / estimatedOutTime / sizeOfMegabyte
    putStrLn ("Getting output streams from server total time:\t" Data.List.++ (show estimatedOutTime) Data.List.++ "s\t(average speed = " Data.List.++ (show speedOut) Data.List.++ "Mb/s)")

    -- Free allocated memory for streams on server
    startTime <- getCurrentTime
    e <- try (Client.free client addressLoopLength) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client addressInMemLoad) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client addressPrecalculations) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client addressDataPairs) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client addressOutCorrelation) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client addressOutIndices) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    putStrLn ("Freeing allocated memory for streams on server:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Close!
    startTime <- getCurrentTime
    tClose transport
    stopTime <- getCurrentTime
    putStrLn ("Closing connection:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Store data
    startTime <- getCurrentTime
    let start = (numTimesteps - 1) * (fromIntegral (Data.Vector.head loopLength)) * correlationNumTopScores * correlationNumPipes

    let correlations = storeData (toList outCorrelation) start 0 0 numTimeseries 0 []
    
    
    return (correlations)

calculateCorrelationCPU :: [[Double]] -> Int -> Int -> Int ->[Double] -> [Double] -> [Double] -> [Double] -> Int ->  Int -> [Double]
calculateCorrelationCPU dataIn numTimeseries windowSize indexCorrelation sums sumsSq sumsXtimesY correlations k 0                     = correlations
calculateCorrelationCPU dataIn numTimeseries windowSize indexCorrelation sums sumsSq sumsXtimesY correlations k iteration             = do
    -- calculate sums
    let oldValsList = if k < windowSize then [ 0.0 | i <- [1 .. numTimeseries]] 
                                        else [(dataIn!!i)!!(k - windowSize) | i <- [0 .. (numTimeseries-1)]]
    let newValsList =  [(dataIn!!i)!!(k) | i <- [0 .. (numTimeseries-1)]] 
    let sumsList = if k == 0 then (Data.List.zipWith (-) newValsList oldValsList) 
                             else Data.List.zipWith (+) sums (Data.List.zipWith (-) newValsList oldValsList)
    let sumsSqList = if k == 0 then Data.List.zipWith (-) (Data.List.map (**2) newValsList) (Data.List.map (**2) oldValsList) 
                               else Data.List.zipWith (+) sumsSq (Data.List.zipWith (-) (Data.List.map (**2) newValsList) (Data.List.map (**2) oldValsList))
    let sumsSquared = Data.List.map (**2) sumsList
    let sumsSqSquared = Data.List.map (**2) sumsSqList
    -- oldX is oldValsList ; newX is newValsList 
    let sumsSndArg =   [ (oldValsList!!j) * (oldValsList!!i) |  i <- [0 .. (numTimeseries-1)], j <- [0 .. (i-1)]]
    let sumsFstArg =   [ (newValsList!!j) * (newValsList!!i) |  i <- [0 .. (numTimeseries-1)], j <- [0 .. (i-1)]]                                                              
    let sumsXtimesYlist = if k == 0 then (Data.List.zipWith (-) sumsFstArg sumsSndArg) 
                                    else (Data.List.zipWith (+) sumsXtimesY (Data.List.zipWith (-) sumsFstArg sumsSndArg))
    let numerator1 = Data.List.map (*(fromIntegral windowSize)) sumsXtimesYlist
    let numerator2 = [ (sumsList!!i) * (sumsList!!j) |  i <- [0 .. (numTimeseries-1)], j <- [0 .. (i-1)] ] 
    let numerator = Data.List.zipWith (-) numerator1 numerator2
    let denominator = [ (1 / sqrt ((fromIntegral windowSize) * sumsSqList!!i - sumsSquared!!i)) * (1 / sqrt ((fromIntegral windowSize) * sumsSqList!!j - sumsSquared!!j))  |  i <- [0 .. (numTimeseries-1)], j <- [0 .. (i-1)]] 
    let correlationsList = Data.List.zipWith (*) numerator denominator
    calculateCorrelationCPU dataIn numTimeseries windowSize 0 sumsList sumsSqList sumsXtimesYlist correlationsList (k+1) (iteration-1)


correlateCPU :: [[Double]] -> Int -> Int -> IO ([Double])
correlateCPU dataIn numTimeseries sizeTimeseries    =    do
    let numTimesteps = sizeTimeseries
    let windowSize = sizeTimeseries
    let numCorrelations = calcNumCorrelations numTimeseries    
    let indexCorrelation = 0
    let iteration = numTimesteps
    let correlationsCpu = calculateCorrelationCPU dataIn numTimeseries windowSize indexCorrelation [] [] [] [] 0 iteration
    
    return correlationsCpu

main = do
    args <- getArgs
    case (Data.List.length args) of
        2 -> return ()
	otherwise -> do
            putStrLn ("Usage: ./CorrelationClient <stream size> <number of streams>")
            exitWith $ ExitFailure (-1)

    let sizeTimeseries = read (args !! 0)::Int
    let numTimeseries = read (args !! 1)::Int

    seed  <- newStdGen
    let dataIn = randomData numTimeseries sizeTimeseries seed
    
    startTime <- getCurrentTime
    correlationsDFE <- correlateDFE dataIn numTimeseries sizeTimeseries
    stopTime <- getCurrentTime

    putStrLn ("DFE correlation total time:\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))
    
    startTime <- getCurrentTime    
    correlationsCPU <- correlateCPU dataIn numTimeseries sizeTimeseries
    stopTime <- getCurrentTime
    putStrLn ("CPU correlation total time:\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    let numCorrelations = calcNumCorrelations numTimeseries
    
    startTime <- getCurrentTime 
    let result = check correlationsCPU correlationsDFE numCorrelations numTimeseries
    stopTime <- getCurrentTime
    putStrLn ("Checking results time:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))
    if result == True then putStrLn ("Test finished succesfully!") else putStrLn ("Test failed!") 
    
