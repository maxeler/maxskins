module Main where

import qualified SimpleService_Client as Client
import Simple_Types

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

simpleCPU :: [Double] -> [Double]
simpleCPU ([])    = []
simpleCPU (x:xs)  = x*x+x : simpleCPU (xs)

check :: Vector Double -> [Double] -> Int -> Int -> [Int] -> [Double] -> [Double] -> (Int, [Int], [Double], [Double])
check outDFE [] status iter iterL outDFEL outCPUL             = (status, iterL, outDFEL, outCPUL)
check outDFE (outCPU:cpul) status iter iterL outDFEL outCPUL 
  | ((fromMaybe 0 ((!?) outDFE 0)) - outCPU) ** 2 > 0.00001 = check (Data.Vector.drop 1 outDFE) cpul (status + 1) (iter  + 1) (iterL Data.List.++ (iter:[])) (outDFEL Data.List.++ ((fromMaybe 0 ((!?) outDFE 0)):[])) (outCPUL Data.List.++ (outCPU:[]))
  | otherwise = check (Data.Vector.drop 1 outDFE) cpul status (iter  + 1) iterL outDFEL outCPUL

printErrors :: [Int] -> [Double] -> [Double] -> String -> String
printErrors [] [] [] output                 = output
printErrors (i:is) (x:xs) (y:ys) output     = printErrors is xs ys (output Data.List.++ "Output data @ " Data.List.++ (show i) Data.List.++ " = " Data.List.++ (show x)  Data.List.++ " (expected " Data.List.++ (show y)  Data.List.++ ")\n")

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
    let size = 1024
    let sizeBytes = size * 4
    let dataIn = [fromIntegral(1)..fromIntegral(size)]
    stopTime <- getCurrentTime
    putStrLn ("Generating input data:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Initialize maxfile
    startTime <- getCurrentTime
    e <- try (Client.simple_init client) :: IO (Either SomeException Int64)
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

    e <- try (Client.malloc_float client (fromIntegral size)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let address_dataIn = getRight e
        
    e <- try (Client.send_data_float client address_dataIn (fromList dataIn)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
        
    stopTime <- getCurrentTime
    putStrLn ("Sending input data:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Allocate memory for output stream on server
    startTime <- getCurrentTime

    e <- try (Client.malloc_float client (fromIntegral size)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let address_dataOut = getRight e

    stopTime <- getCurrentTime
    putStrLn ("Allocating memory for output stream on server:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

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

    e <- try (Client.max_queue_input client actions (pack "x") address_dataIn (fromIntegral sizeBytes)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_queue_output client actions (pack "y") address_dataOut (fromIntegral sizeBytes)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_run client engine actions) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    stopTime <- getCurrentTime
    putStrLn ("Simple time:\t\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

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
    dataOutDFE <- Client.receive_data_float client address_dataOut (fromIntegral size)
    stopTime <- getCurrentTime
    putStrLn ("Getting output stream:\t(size = " Data.List.++ (show (size * 32)) Data.List.++ " bit)\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    --Free allocated memory for streams on server
    startTime <- getCurrentTime

    e <- try (Client.free client address_dataIn) :: IO (Either SomeException ())
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
    e <- try (Client.simple_free client) :: IO (Either SomeException ())
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
    putStrLn ("DFE simple total time:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startDFETime)))

    -- CPU Output
    startTime <- getCurrentTime
    let dataOutCPU = simpleCPU dataIn
    stopTime <- getCurrentTime
    putStrLn ("CPU simple total time::\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))   

    -- Checking results
    startTime <- getCurrentTime
    let (status, iter, dataErrDFE, dataErrCPU) = check dataOutDFE dataOutCPU 0 0 [] [] []
    putStr (printErrors iter dataErrDFE dataErrCPU [])
    stopTime <- getCurrentTime
    putStrLn ("Checking results:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    if (status == 0)
        then putStrLn ("Test successful!")
        else do putStrLn ("Test failed "  Data.List.++ show status  Data.List.++ " times!")
                exitWith $ ExitFailure (-1)
