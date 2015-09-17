import unittest
import sys

sys.path.append('..')
from parse import *
from format import *

class TestParse(unittest.TestCase):

    def setUp(self):
        pass

    # Test for parse_functions(code)
    def test_parse_functions_1(self):
        input = '''
                void  VectorAddition_writeLMem(
                       int64_t 		 param_address,
                       int64_t   param_nbytes,
                       const int32_t *instream_cpu_to_lmem);
                '''
        output = [{'type': 'void', 'name': 'VectorAddition_writeLMem', 
                   'arguments': [{'type': 'int64_t', 'name': 'param_address'}, 
                                 {'type': 'int64_t', 'name': 'param_nbytes'},
                                 {'type': 'const int32_t*', 'name': 'instream_cpu_to_lmem'}]}]
        self.assertEqual(parse_functions(prepare_for_parsing(input)), output)

    # Tests for parse_enums(code)
    def test_parse_enums_1(self):
        input = '''
                 typedef enum max_config_key_int64 {
                     MAX_CONFIG_PCIE_TIMEOUT,		
                     MAX_CONFIG_WFI_TIMEOUT,		    
                     MAX_CONFIG_TOPOLOGY_TIMEOUT,	
                     MAX_CONFIG_DEBUG_MODE,          
                     MAX_CONFIG_SHUTDOWN_TIMEOUT,	
                     MAX_CONFIG_DFEPRINTF_TIMEOUT_CYCLES_SIMULATION, 
                     MAX_CONFIG_DFEPRINTF_TIMEOUT_CYCLES_DFE, 
                     MAX_CONFIG_NB_INTEGERS 		
                 } max_config_key_int64_t;
                 '''
        output = [{'elements': [{'name': 'MAX_CONFIG_PCIE_TIMEOUT'}, 
                                {'name': 'MAX_CONFIG_WFI_TIMEOUT'}, 
                                {'name': 'MAX_CONFIG_TOPOLOGY_TIMEOUT'},
                                {'name': 'MAX_CONFIG_DEBUG_MODE'}, 
                                {'name': 'MAX_CONFIG_SHUTDOWN_TIMEOUT'}, 
                                {'name': 'MAX_CONFIG_DFEPRINTF_TIMEOUT_CYCLES_SIMULATION'},
                                {'name': 'MAX_CONFIG_DFEPRINTF_TIMEOUT_CYCLES_DFE'}, 
                                {'name': 'MAX_CONFIG_NB_INTEGERS'}],
                   'name': 'max_config_key_int64_t'}]
        self.assertEqual(parse_enums(prepare_for_parsing(input)), output)

    # Test for parse_structs(code)
    def test_parse_structs_1(self):
        input = '''
                typedef struct event_atomic {
                       uint32_t        event_id;
                } event_atomic_t;
                '''
        output = [{'name': 'event_atomic_t', 
                   'arguments': [{'type': 'uint32_t', 'name': 'event_id'}]}]
        self.assertEqual(parse_structs(prepare_for_parsing(input)), output)

    # Test for parse_typedefs(code)
    def test_parse_typedefs_1(self):
        input = '''
                typedef unsigned char u_int8;
                '''
        output = [{'type' : 'unsigned char', 'name' : 'u_int8'}]
        self.assertEqual(parse_typedefs(prepare_for_parsing(input)), output)

if __name__ == '__main__':
    unittest.main()
