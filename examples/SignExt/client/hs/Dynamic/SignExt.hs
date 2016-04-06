module Main where

import qualified SignExtService_Client as Client
import SignExt_Types
import SignExtService

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
import System.Environment
import Data.Time
import Data.Text.Lazy
import Data.Vector
import Network
import System.Exit
import System.Random
import Text.Printf
import Control.Concurrent
import Numeric (showHex, showIntAtBase)


-- Package size.
package_size = 3
-- Buffer size. 
buffer_size = 4096 * 512
-- Buffer aligment.
buffer_alignment = 4096
-- Thread sleep time.
thread_sleep_ime = 1
-- Size of int in bits.
size_of_int = 32

getRight           :: Either left right -> right
getRight (Right x) = x

-- stream read function
stream_read x to_cpu y frame_address fsz_address =  do                                                       
					e <- try (Client.max_framed_stream_read x to_cpu y frame_address fsz_address) :: IO (Either SomeException Int64)
					case e of
					     Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
					     Right ex -> return ()
					let ret = getRight e
					if  (fromIntegral ret) == 0 then do
                                                                         threadDelay thread_sleep_ime
                                                                         (stream_read x to_cpu y frame_address fsz_address)                                                  
							            else return (fromIntegral ret)
-- proces data function
process_data x fsz_address frame_address to_cpu numRx = do
					fsz_array <- Client.receive_data_int64_t x fsz_address (fromIntegral 1)
					let fsz = Data.Vector.head fsz_array
					putStrLn ("CPU: Got output frame " Data.List.++ (show numRx) Data.List.++ " - " Data.List.++(show fsz) Data.List.++ " bytes")
					frame_array <- Client.receive_data_int64_t x frame_address (fromIntegral 1)
					let frame = Data.Vector.head frame_array
					word_array <- Client.receive_data_int64_t x frame package_size					    
					returnVal <- process_words word_array package_size 0 numRx
					discardVal <- Client.max_framed_stream_discard x to_cpu (fromIntegral 1);
					if (returnVal) == 0 then return 0
							else do 
							       stream_read x to_cpu (fromIntegral 1) frame_address fsz_address
							       process_data x fsz_address frame_address to_cpu (numRx + 1)

-- proces words function
process_words word_array 0 i  numRx           = do 
                                         if (word_array ! 0 ) == 0 && (word_array ! 1 ) == 0   && (word_array ! 2 ) == 0 then return 0
                                                                                                                         else return 1
process_words word_array package_size i numRx = do
					 let word = (word_array ! i)		                                 
					 putStr ("FRAME[" Data.List.++ (show numRx) Data.List.++ "] WORD[" Data.List.++ (show i) Data.List.++ "]: ")
					 putStrLn $ printf "0x%08x" word                                        
					 process_words word_array (package_size - 1) (i + 1) numRx
				                                   
main = do
    startTime <- getCurrentTime
    startDFETime <- getCurrentTime
    args <- getArgs
    
    case (Data.List.length args) of
        2 -> return ()
	otherwise -> do
            putStrLn ("Usage: dfe_ip remote_ip")
            exitWith $ ExitFailure (-1)

    let dfe_ip = (args !! 0)
    
    let remote_ip = (args !! 1)
        
    -- Make socket
    transport <- hOpen ("localhost", PortNumber 9090)

    -- Wrap in a protocol
    let protocol = BinaryProtocol transport

    -- Create a client to use the protocol encoder
    let client = (protocol, protocol)
    stopTime <- getCurrentTime
    putStrLn ("Creating a client and opening connection:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Allocate Dfe ip adress
    e <- try (Client.malloc_int64_t (protocol, protocol) (fromIntegral 5)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let dfe_ip_address = getRight e

    e <- try (Client.inet_aton client (pack dfe_ip) dfe_ip_address) :: IO (Either SomeException Int32)
    case e of
        Left ex -> putStrLn $ "Caught exception inet_aton: " Data.List.++ show ex
        Right ex -> return ()
    let dfe_ip_address_aligned = getRight e

    -- Allocate Remote ip adress
    e <- try (Client.malloc_int64_t client (fromIntegral 5)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let remote_ip_address = getRight e

    Client.inet_aton client  (pack remote_ip) remote_ip_address

    -- Allocate Netmask address
    startTime <- getCurrentTime
    e <- try (Client.malloc_int64_t client (fromIntegral 5)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let netmask_address = getRight e

    Client.inet_aton client (pack "255.255.255.0") netmask_address

    -- Initialize maxfile
    startTime <- getCurrentTime
    e <- try (Client.signExt_init client) :: IO (Either SomeException Int64)
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

    -- Set Enum
    let enumKey = Max_config_key_bool_t_struct (Just MAX_CONFIG_PRINTF_TO_STDOUT)
   
    e <- try (Client.max_config_set_bool client (enumKey) (1))  :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Set actions
    e <- try (Client.max_actions_init client maxfile (pack "default")) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let actions = getRight e

    -- Run actions
    e <- try (Client.max_run client engine actions) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Free actions
    e <- try (Client.max_actions_free client actions) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Allocate buffer address
    startTime <- getCurrentTime
    e <- try (Client.malloc_int64_t client (fromIntegral 1)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let buffer_address = getRight e

    e <- try (Client.posix_memalign client buffer_address buffer_alignment buffer_size) :: IO (Either SomeException Int32)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let buffer_address_aligned = getRight e


    ---- Read buffer
    buffer_array <- Client.receive_data_int64_t client (fromIntegral buffer_address) (fromIntegral 1)
    let buffer = Data.Vector.head buffer_array

    -- Framed stream setup
    to_cpu <- Client.max_framed_stream_setup client engine (pack "toCPU") buffer buffer_size (fromIntegral (-1))

    -- Max_net_connection
    let enumconn = Max_net_connection_t_struct (Just MAX_NET_CONNECTION_QSFP_TOP_10G_PORT1)
   
    --- Ip config
    e <- try (Client.max_ip_config client engine enumconn (fromIntegral dfe_ip_address) netmask_address) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Udp create socket
    e <- try (Client.max_udp_create_socket client engine (pack "udpTopPort1")) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let dfe_socket = getRight e

    -- Socket bind
    let port = 2000
    e <- try (Client.max_udp_bind client dfe_socket port) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Udp connect
    e <- try (Client.max_udp_connect client dfe_socket remote_ip_address (fromIntegral 0)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    putStrLn ("Listening on: " Data.List.++  dfe_ip Data.List.++ " port " Data.List.++ (show port))

    putStrLn ("Waiting for kernel response...")

    -- Allocate memory for frame address
    e <- try (Client.malloc_int64_t client (fromIntegral 1)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let frame_address = getRight e

    -- Allocate memory for fsz address
    e <- try (Client.malloc_int64_t client (fromIntegral 1)) :: IO (Either SomeException Int64)
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    let fsz_address = getRight e

    -- Main loop, wait for packages
    let numMessageRx = 0
 
    framed_return <- stream_read client to_cpu 1 frame_address fsz_address

    val <- process_data client fsz_address frame_address to_cpu numMessageRx

    -- Close sockets
    e <- try (Client.max_udp_close client dfe_socket) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    e <- try (Client.max_framed_stream_release client to_cpu) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Unload DFE
    startTime <- getCurrentTime
    e <- try (Client.max_unload client engine) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    stopTime <- getCurrentTime
    putStrLn ("Unloading DFE:\t\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    e <- try (Client.max_file_free client maxfile) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Free allocated memory for streams on server
    startTime <- getCurrentTime
    e <- try (Client.free client (fromIntegral dfe_ip_address)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client remote_ip_address) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client netmask_address) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client buffer_address) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    e <- try (Client.free client (fromIntegral buffer_address_aligned)) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()
    
    stopTime <- getCurrentTime
    putStrLn ("Freeing allocated memory for streams on server:\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

    -- Free allocated maxfile data
    startTime <- getCurrentTime
    e <- try (Client.signExt_free client) :: IO (Either SomeException ())
    case e of
        Left ex -> putStrLn $ "Caught exception: " Data.List.++ show ex
        Right ex -> return ()

    -- Close!
    startTime <- getCurrentTime
    tClose transport
    stopTime <- getCurrentTime
    putStrLn ("Closing connection:\t\t\t\t" Data.List.++ (show (diffUTCTime stopTime startTime)))

