module Main where

import qualified VectorAdditionService_Client as Client
import VectorAddition_Types

import Thrift
import Thrift.Protocol.Binary
import Thrift.Server
import Thrift.Transport
import Thrift.Transport.Handle

import Control.Exception
import Data.Either
import Data.Int
import Data.List
import Data.Maybe
import Data.Time
import Data.Text.Lazy
import Data.Vector
import Network
import System.Exit
import System.Random
import Text.Printf

getRight           :: Either left right -> right
getRight (Right x) = x

randomlist :: Int -> StdGen -> [Int32]
randomlist  n = Data.List.take n . Data.List.unfoldr (Just . randomR (1, 1000))

vectorAdditionCPU :: [Int32] -> [Int32] -> Int32 -> [Int32]
vectorAdditionCPU [] [] s    = []
vectorAdditionCPU (x:xs) (y:ys) s = (x + y + s) : (vectorAdditionCPU (xs) (ys) s)

check :: [Int32] -> [Int32] -> Int -> Int -> [Int]
check [] [] start end = []
check (x:outDFE) (y:outCPU) start end
	| (x == y)		=  check outDFE outCPU (start+1) end
	| otherwise 	= (start) : check outDFE outCPU (start+1) end

printErrors :: [Int] -> [Int32] -> [Int32] -> String -> String
printErrors [] xs ys output                 = output
printErrors (i:is) xs ys output     = printErrors is xs ys (output Data.List.++ "Output data @ " Data.List.++ (show i) Data.List.++ " = " Data.List.++ (show (xs!!i))  Data.List.++ " (expected " Data.List.++ (show (ys!!i))  Data.List.++ ")\n")

main = do
    startTime <- getCurrentTime
    startDFETime <- getCurrentTime
    
    -- Make socket
    transport <- hOpen ("localhost", PortNumber 9090)

    -- Wrap in a protocol
    let protocol = BinaryProtocol transport

    -- Create a client to use the protocol encoder
    let client = (protocol, protocol)
    stopTime <- getCurrentTime
    putStrLn ("Creating a client and opening connection:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Generate input data
    startTime <- getCurrentTime
    let size = 384
    let sizeBytes = size * 4
    let scalar = 3
    seed  <- newStdGen
    let x = randomlist size seed
    seed  <- newStdGen
    let y = randomlist size seed
    stopTime <- getCurrentTime
    putStrLn ("Generating input data:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Initialize maxfile
    startTime <- getCurrentTime
    e <- try (Client.vectorAddition_init client) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let maxfile = getRight e
    stopTime <- getCurrentTime
    putStrLn ("Initializing maxfile:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Load DFE
    startTime <- getCurrentTime
    e <- try (Client.max_load client maxfile (pack "*")) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let engine = getRight e
    stopTime <- getCurrentTime
    putStrLn ("Loading DFE:\t\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Allocate and send input streams to server
    startTime <- getCurrentTime

    e <- try (Client.malloc_int32_t client (fromIntegral size)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let address_x = getRight e

    e <- try (Client.send_data_int32_t client address_x (fromList x)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.malloc_int32_t client (fromIntegral size)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let address_y = getRight e

    e <- try (Client.send_data_int32_t client address_y (fromList y)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
        
    stopTime <- getCurrentTime
    putStrLn ("Sending input data:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Allocate memory for output stream on server
    startTime <- getCurrentTime

    e <- try (Client.malloc_int32_t client (fromIntegral size)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let address_dataOut = getRight e

    stopTime <- getCurrentTime
    putStrLn ("Allocating memory for output stream on server:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Writing to LMem
    startTime <- getCurrentTime

    e <- try (Client.max_actions_init client maxfile (pack "writeLMem")) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let actions = getRight e

    e <- try (Client.max_set_param_uint64t client actions (pack "address") (fromIntegral 0)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_set_param_uint64t client actions (pack "nbytes") (fromIntegral sizeBytes)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_queue_input client actions (pack "cpu_to_lmem") address_x (fromIntegral sizeBytes)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_run client engine actions) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    stopTime <- getCurrentTime
    putStrLn ("Writing to LMem:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Action default
    startTime <- getCurrentTime
    e <- try (Client.max_actions_init client maxfile (pack "default")) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let actions = getRight e

    e <- try (Client.max_set_param_uint64t client actions (pack "N") (fromIntegral size)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_set_param_uint64t client actions (pack "A") (fromIntegral scalar)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_queue_input client actions (pack "y") address_y (fromIntegral sizeBytes)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_queue_output client actions (pack "s") address_dataOut (fromIntegral sizeBytes)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_run client engine actions) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
 
    stopTime <- getCurrentTime
    putStrLn ("Vector addition time:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Unload DFE
    startTime <- getCurrentTime
    e <- try (Client.max_unload client engine) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    putStrLn ("Unloading DFE:\t\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Get output stream from server
    startTime <- getCurrentTime
    dataOutDFE <- Client.receive_data_int32_t client address_dataOut (fromIntegral size)
    stopTime <- getCurrentTime
    putStrLn ("Getting output stream:\t(size = " Data.List.++ (show (size * 32)) Data.List.++ " bit)\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Free allocated memory for streams on server
    startTime <- getCurrentTime
    e <- try (Client.free client address_x) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    startTime <- getCurrentTime
    e <- try (Client.free client address_y) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.free client address_dataOut) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    
    stopTime <- getCurrentTime
    putStrLn ("Freeing allocated memory for streams on server:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Free allocated maxfile data
    startTime <- getCurrentTime
    e <- try (Client.vectorAddition_free client) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    stopTime <- getCurrentTime
    putStrLn ("Freeing allocated maxfile data:\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Close!
    startTime <- getCurrentTime
    tClose transport
    stopTime <- getCurrentTime
    putStrLn ("Closing connection:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    stopTime <- getCurrentTime
    putStrLn ("DFE vector addition total time:\t\t\t" Data.List.++ (show (diffUTCTime stopTime startDFETime)))

    -- CPU Output
    startTime <- getCurrentTime
    let dataOutCPU = vectorAdditionCPU x y scalar
    stopTime <- getCurrentTime
    putStrLn ("CPU vector addition total time::\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Checking results
    startTime <- getCurrentTime
    let errors = check (toList dataOutDFE) dataOutCPU 0 size
    stopTime <- getCurrentTime
    putStrLn ("Checking results:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    if ((Data.List.length errors)== 0)
	then putStrLn ("Test successful!")
        else do putStr (printErrors errors (toList dataOutDFE) dataOutCPU [])
                putStrLn ("Test failed "  Data.List.++ show (Data.List.length errors)  Data.List.++ " times!")
                exitWith $ ExitFailure (-1)


