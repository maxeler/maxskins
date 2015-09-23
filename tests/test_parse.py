"""
Parse functions unit tests.
"""
import unittest
import sys

sys.path.append('..')
from parse import parse_functions, parse_enums, parse_structs, parse_typedefs
from parse import parse
from format import prepare_for_parsing

class TestParse(unittest.TestCase):
    """
    Parse functions unittest class.
    """
    def setUp(self):
        pass

    # Test for parse_functions(code)
    def test_parse_functions_1(self):
        """parse_functions Unit test 1: No functions"""
        input1 = '''
                 void  VectorAddition_writeLMem{
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem};
                 '''
        output1 = []
        self.assertEqual(parse_functions(prepare_for_parsing(input1)), output1)

    def test_parse_functions_2(self):
        """parse_functions Unit test 2: One function"""
        input2 = '''
                 void  VectorAddition_writeLMem(
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem);
                 '''
        output2 = [{'type': 'void',
                    'name': 'VectorAddition_writeLMem',
                    'arguments': [{'type': 'int64_t',
                                   'name': 'param_address'},
                                  {'type': 'int64_t',
                                   'name': 'param_nbytes'},
                                  {'type': 'const int32_t*',
                                   'name': 'instream_cpu_to_lmem'}]}]
        self.assertEqual(parse_functions(prepare_for_parsing(input2)), output2)

    def test_parse_functions_3(self):
        """parse_functions Unit test 3: Several functions"""
        input3 = '''
                 void  VectorAddition_writeLMem(
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem);

                 int a();
                 
                 double* b();
                 '''
        output3 = [{'type': 'void',
                    'name': 'VectorAddition_writeLMem',
                    'arguments': [{'type': 'int64_t',
                                   'name': 'param_address'},
                                  {'type': 'int64_t',
                                   'name': 'param_nbytes'},
                                  {'type': 'const int32_t*',
                                   'name': 'instream_cpu_to_lmem'}]},
                   {'type': 'int', 'name': 'a', 'arguments': []},
                   {'type': 'double*', 'name': 'b', 'arguments': []}]
        self.assertEqual(parse_functions(prepare_for_parsing(input3)), output3)

    def test_parse_functions_4(self):
        """parse_functions Unit test 4: Array arguments"""
        input3 = 'int a(int b[5]);'
        output3 = [{'type': 'int', 'name': 'a', 'arguments': [{'type': 'int',
                                                               'name': 'b',
                                                               'array': '5'}]}]
        self.assertEqual(parse_functions(prepare_for_parsing(input3)), output3)

    # Tests foall lines end with ;r parse_enums(code)
    def test_parse_enums_1(self):
        """parse_enums Unit test 1: No enums"""
        input1 = '''
                 void  VectorAddition_writeLMem{
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem};
                 '''
        output1 = []
        self.assertEqual(parse_enums(prepare_for_parsing(input1)), output1)

    def test_parse_enums_2(self):
        """parse_enums Unit test 2: One enum"""
        input2 = '''
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
        output2 = [
            {'elements':
             [{'name': 'MAX_CONFIG_PCIE_TIMEOUT'},
              {'name': 'MAX_CONFIG_WFI_TIMEOUT'},
              {'name': 'MAX_CONFIG_TOPOLOGY_TIMEOUT'},
              {'name': 'MAX_CONFIG_DEBUG_MODE'},
              {'name': 'MAX_CONFIG_SHUTDOWN_TIMEOUT'},
              {'name': 'MAX_CONFIG_DFEPRINTF_TIMEOUT_CYCLES_SIMULATION'},
              {'name': 'MAX_CONFIG_DFEPRINTF_TIMEOUT_CYCLES_DFE'},
              {'name': 'MAX_CONFIG_NB_INTEGERS'}],
             'name': 'max_config_key_int64_t'}
        ]
        self.assertEqual(parse_enums(prepare_for_parsing(input2)), output2)

    def test_parse_enums_3(self):
        """parse_enums Unit test 3: Several enums"""
        input3 = '''
                 typedef enum{aa} a;
                 typedef enum bbb {bb} b;
                 '''
        output3 = [{'elements': [{'name': 'aa'}], 'name': 'a'},
                   {'elements': [{'name': 'bb'}], 'name': 'b'}]
        self.assertEqual(parse_enums(prepare_for_parsing(input3)), output3)

    def test_parse_enums_4(self):
        """parse_enums Unit test 4: Elemate with value"""
        input4 = '''
                 typedef enum{aa=5} a;
                 '''
        output4 = [{'elements': [{'name': 'aa', 'value': '5'}], 'name': 'a'}]
        self.assertEqual(parse_enums(prepare_for_parsing(input4)), output4)

    # Test for parse_structs(code)
    def test_parse_structs_1(self):
        """parse_structs Unit test 1: No structs"""
        input1 = '''
                 void  VectorAddition_writeLMem{
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem};
                 '''
        output1 = []
        self.assertEqual(parse_structs(prepare_for_parsing(input1)), output1)

    def test_parse_structs_2(self):
        """parse_structs Unit test 2: One struct"""
        input2 = '''
                 typedef struct event_atomic {
                        uint32_t        event_id;
                 } event_atomic_t;
                 '''
        output2 = [{'name': 'event_atomic_t',
                    'arguments': [{'type': 'uint32_t', 'name': 'event_id'}]}]
        self.assertEqual(parse_structs(prepare_for_parsing(input2)), output2)

    def test_parse_structs_3(self):
        """parse_structs Unit test 2: Several struct"""
        input3 = '''
                 typedef struct event_atomic {
                        uint32_t        event_id;
                 } event_atomic_t;

                 typedef struct event_atomic {
                        uint32_t        event_id;
                 } event_atomic_t;

                 typedef struct event_atomic {
                        uint32_t        event_id;
                 } event_atomic_t;
                 '''
        output3 = [{'name': 'event_atomic_t',
                    'arguments': [{'type': 'uint32_t', 'name': 'event_id'}]},
                   {'name': 'event_atomic_t',
                    'arguments': [{'type': 'uint32_t', 'name': 'event_id'}]},
                   {'name': 'event_atomic_t',
                    'arguments': [{'type': 'uint32_t', 'name': 'event_id'}]}]
        self.assertEqual(parse_structs(prepare_for_parsing(input3)), output3)

    def test_parse_structs_4(self):
        """parse_structs Unit test 4: Array argument"""
        input2 = '''
                 struct event_atomic {
                        uint32_t        event_id[10];
                 };
                 '''
        output2 = [{'name': 'struct event_atomic',
                    'arguments': [{'type': 'uint32_t',
                                   'name': 'event_id',
                                   'array': '10'}]}]
        self.assertEqual(parse_structs(prepare_for_parsing(input2)), output2)

    # Test for parse_typedefs(code)
    def test_parse_typedefs_1(self):
        """parse_typedefs Unit test 1: No typedefs"""
        input1 = '''
                 void  VectorAddition_writeLMem{
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem};
                 '''
        output1 = []
        self.assertEqual(parse_typedefs(prepare_for_parsing(input1)), output1)

    def test_parse_typedefs_2(self):
        """parse_typedefs Unit test 2: One typedef"""
        input2 = '''
                 typedef unsigned char u_int8;
                 '''
        output2 = [{'type' : 'unsigned char', 'name' : 'u_int8'}]
        self.assertEqual(parse_typedefs(prepare_for_parsing(input2)), output2)

    def test_parse_typedefs_3(self):
        """parse_typedefs Unit test 3: Several typedefs"""
        input3 = '''
                 typedef unsigned char u_int8;
                 typedef unsigned char u_int8;
                 '''
        output3 = [{'type' : 'unsigned char', 'name' : 'u_int8'},
                   {'type' : 'unsigned char', 'name' : 'u_int8'}]
        self.assertEqual(parse_typedefs(prepare_for_parsing(input3)), output3)

    # Test for parse(code)
    def test_parse_1(self):
        """parse Unit test 1: Nothing to parse"""
        input1 = '''
                 void  VectorAddition_writeLMem{
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem};
                 '''
        output1 = {'functions': [], 'enums': [], 'typedefs': [], 'structs': []}
        self.assertEqual(parse(input1), output1)
    def test_parse_2(self):
        """parse Unit test 2: Parse all"""
        input2 = '''
                 void  VectorAddition_writeLMem(
                        int64_t 		 param_address,
                        int64_t   param_nbytes,
                        const int32_t *instream_cpu_to_lmem);
                 typedef unsigned char u_int8;

                 typedef enum max_config_key_int64 {
                     MAX_CONFIG_PCIE_TIMEOUT,
                     MAX_CONFIG_WFI_TIMEOUT,
                     MAX_CONFIG_TOPOLOGY_TIMEOUT,
                     MAX_CONFIG_DEBUG_MODE,
                     MAX_CONFIG_SHUTDOWN_TIMEOUT,
                     MAX_CONFIG_DFEPRINTF_TIMEOUT_1,
                     MAX_CONFIG_DFEPRINTF_TIMEOUT_2,
                     MAX_CONFIG_NB_INTEGERS
                 } max_config_key_int64_t;

                 typedef struct event_atomic {
                        uint32_t        event_id;
                 } event_atomic_t;
                 '''
        output2 = {'typedefs': [{'type' : 'unsigned char', 'name' : 'u_int8'}],
                   'functions': [{'type': 'void',
                                  'name': 'VectorAddition_writeLMem',
                                  'arguments':
                                      [{'type': 'int64_t',
                                        'name': 'param_address'},
                                       {'type': 'int64_t',
                                        'name': 'param_nbytes'},
                                       {'type': 'const int32_t*',
                                        'name': 'instream_cpu_to_lmem'}]}],
                   'enums': [{'elements':
                                  [{'name': 'MAX_CONFIG_PCIE_TIMEOUT'},
                                   {'name': 'MAX_CONFIG_WFI_TIMEOUT'},
                                   {'name': 'MAX_CONFIG_TOPOLOGY_TIMEOUT'},
                                   {'name': 'MAX_CONFIG_DEBUG_MODE'},
                                   {'name': 'MAX_CONFIG_SHUTDOWN_TIMEOUT'},
                                   {'name': 'MAX_CONFIG_DFEPRINTF_TIMEOUT_1'},
                                   {'name': 'MAX_CONFIG_DFEPRINTF_TIMEOUT_2'},
                                   {'name': 'MAX_CONFIG_NB_INTEGERS'}],
                              'name': 'max_config_key_int64_t'}],
                   'structs': [{'name': 'event_atomic_t',
                                'arguments': [{'type': 'uint32_t',
                                               'name': 'event_id'}]}]}
        self.assertEqual(parse(input2), output2)

if __name__ == '__main__':
    unittest.main()

