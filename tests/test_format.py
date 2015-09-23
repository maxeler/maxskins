"""
Format functions unit tests.
"""
import unittest
import sys

sys.path.append('..')
from format import remove_duplicates, transform, prepare_for_parsing

class TestFormat(unittest.TestCase):
    """
    Format functions unittest class.
    """
    def setUp(self):
        pass

    # Tests for remove_duplicates_from_list(seq)
    def test_remove_duplicates_1(self):
        """remove_duplicates Unit test 1: Removing int duplicates"""
        input1 = [1, 2, 2, 6, 4, 6, 1]
        output1 = [1, 2, 6, 4]
        self.assertEqual(remove_duplicates(input1), output1)

    def test_remove_duplicates_2(self):
        """remove_duplicates Unit test 2: Removing string duplicates"""
        input2 = ['a', 'b', 'c', 'c']
        output2 = ['a', 'b', 'c']
        self.assertEqual(remove_duplicates(input2), output2)

    def test_remove_duplicates_3(self):
        """remove_duplicates Unit test 3: No duplicates"""
        input3 = ['a', 'b', 'c']
        output3 = ['a', 'b', 'c']
        self.assertEqual(remove_duplicates(input3), output3)

    def test_remove_duplicates_4(self):
        """remove_duplicates Unit test 4: Removing mixed duplicates"""
        input4 = ['a', 'a', '1', '1']
        output4 = ['a', '1']
        self.assertEqual(remove_duplicates(input4), output4)

    # Tests for transform(code, transformations)
    def test_transform_1(self):
        """transform Unit test 1: No transformations"""
        input1 = 'This is a test'
        transformations1 = []
        output1 = 'This is a test'
        self.assertEqual(transform(input1, transformations1), output1)

    def test_transform_2(self):
        """transform Unit test 2: One transformation"""
        input2 = 'This is a test'
        transformations2 = [['test', 'test!']]
        output2 = 'This is a test!'
        self.assertEqual(transform(input2, transformations2), output2)

    def test_transform_3(self):
        """transform Unit test 3: Multiple transformations"""
        input3 = 'This is a test'
        transformations3 = [['is', 'was'], ['test', 'test!']]
        output3 = 'Thwas was a test!'
        self.assertEqual(transform(input3, transformations3), output3)

    def test_transform_4(self):
        """transform Unit test 4: Multiple transformations"""
        input4 = 'a   b nna'
        transformations4 = [['a', 'b'], ['  ', ' '], ['nb', 'bnb']]
        output4 = 'b  b nbnb'
        self.assertEqual(transform(input4, transformations4), output4)

    # Tests for prepare_for_parsing(code)
    def test_prepare_for_parsing_01(self):
        """prepare_for_parsing Unit test 1: Removing empty lines"""
        input1 = '''hi
                   bye'''
        output1 = 'hi bye'
        self.assertEqual(prepare_for_parsing(input1), output1)

    def test_prepare_for_parsing_02(self):
        """prepare_for_parsing Unit test 2: Removing lines with #"""
        input2 = '''hi
                   # bye'''
        output2 = 'hi'
        self.assertEqual(prepare_for_parsing(input2), output2)

    def test_prepare_for_parsing_03(self):
        """prepare_for_parsing Unit test 3: Removing excess ' '"""
        input3 = '            '
        output3 = ''
        self.assertEqual(prepare_for_parsing(input3), output3)

    def test_prepare_for_parsing_04(self):
        """prepare_for_parsing Unit test 4: Lines must end with ;"""
        input4 = 'int a; int b;'
        output4 = 'int a;\nint b;\n'
        self.assertEqual(prepare_for_parsing(input4), output4)

    def test_prepare_for_parsing_05(self):
        """prepare_for_parsing Unit test 5: Removing ' ' before *"""
        input5 = 'df **  fl** '
        output5 = 'df** fl**'
        self.assertEqual(prepare_for_parsing(input5), output5)

    def test_prepare_for_parsing_06(self):
        """prepare_for_parsing Unit test 6: Removing ' ' around ;"""
        input6 = 'a ; b'
        output6 = 'a;\nb'
        self.assertEqual(prepare_for_parsing(input6), output6)

    def test_prepare_for_parsing_07(self):
        """prepare_for_parsing Unit test 7: Removing ' ' around ,"""
        input7 = 'a , b'
        output7 = 'a,b'
        self.assertEqual(prepare_for_parsing(input7), output7)

    def test_prepare_for_parsing_08(self):
        """prepare_for_parsing Unit test 8: Removing ' ' around ("""
        input8 = 'a ( b'
        output8 = 'a(b'
        self.assertEqual(prepare_for_parsing(input8), output8)

    def test_prepare_for_parsing_09(self):
        """prepare_for_parsing Unit test 9: Removing ' ' around )"""
        input9 = 'a ) b'
        output9 = 'a)b'
        self.assertEqual(prepare_for_parsing(input9), output9)

    def test_prepare_for_parsing_10(self):
        """prepare_for_parsing Unit test 10: Removing ' ' around {"""
        input10 = 'a { b'
        output10 = 'a{b'
        self.assertEqual(prepare_for_parsing(input10), output10)

    def test_prepare_for_parsing_11(self):
        """prepare_for_parsing Unit test 11: Removing ' ' around }"""
        input11 = 'a } b'
        output11 = 'a}b'
        self.assertEqual(prepare_for_parsing(input11), output11)

    def test_prepare_for_parsing_12(self):
        """prepare_for_parsing Unit test 12: Removing ' ' around ="""
        input12 = 'a = b'
        output12 = 'a=b'
        self.assertEqual(prepare_for_parsing(input12), output12)

    def test_prepare_for_parsing_13(self):
        """prepare_for_parsing Unit test 13: Removing ' ' before ["""
        input13 = 'a [ b'
        output13 = 'a[ b'
        self.assertEqual(prepare_for_parsing(input13), output13)

    def test_prepare_for_parsing_14(self):
        """prepare_for_parsing Unit test 14: Removing extern"""
        input14 = 'extern a'
        output14 = 'a'
        self.assertEqual(prepare_for_parsing(input14), output14)

    def test_prepare_for_parsing_15(self):
        """prepare_for_parsing Unit test 15: Removing __restrict"""
        input15 = '__restrict a'
        output15 = ' a'
        self.assertEqual(prepare_for_parsing(input15), output15)

    def test_prepare_for_parsing_16(self):
        """prepare_for_parsing Unit test 16: Removing __signed__"""
        input16 = '__signed__ a'
        output16 = 'a'
        self.assertEqual(prepare_for_parsing(input16), output16)

    def test_prepare_for_parsing_17(self):
        """prepare_for_parsing Unit test 17: Removing __extension__"""
        input17 = '__extension__ a'
        output17 = 'a'
        self.assertEqual(prepare_for_parsing(input17), output17)

    def test_prepare_for_parsing_18(self):
        """prepare_for_parsing Unit test 18: Removing __inline"""
        input18 = '__inline a'
        output18 = 'a'
        self.assertEqual(prepare_for_parsing(input18), output18)

    def test_prepare_for_parsing_19(self):
        """prepare_for_parsing Unit test 19: Removing volatile"""
        input19 = 'volatile a'
        output19 = 'a'
        self.assertEqual(prepare_for_parsing(input19), output19)

    def test_prepare_for_parsing_20(self):
        """prepare_for_parsing Unit test 20: (void) --> ()"""
        input20 = '(void)'
        output20 = '()'
        self.assertEqual(prepare_for_parsing(input20), output20)

    def test_prepare_for_parsing_21(self):
        """prepare_for_parsing Unit test 21: [] --> [ ]"""
        input21 = '[]'
        output21 = '[ ]'
        self.assertEqual(prepare_for_parsing(input21), output21)

    def test_prepare_for_parsing_22(self):
        """prepare_for_parsing Unit test 22: Struct must be in one line"""
        input22 = '''struct a{
                         int a;
                         int b;};'''
        output22 = 'struct a{int a;int b;};\n'
        self.assertEqual(prepare_for_parsing(input22), output22)

    def test_prepare_for_parsing_23(self):
        """prepare_for_parsing Unit test 23: Enum must be in one line"""
        input23 = '''enum a{
                         int a;
                         int b;};'''
        output23 = 'enum a{int a;int b;};\n'
        self.assertEqual(prepare_for_parsing(input23), output23)

    def test_prepare_for_parsing_24(self):
        """prepare_for_parsing Unit test 24: Removing __attribute__"""
        input24 = '''int a()__attribute__(());'''
        output24 = 'int a();\n'
        self.assertEqual(prepare_for_parsing(input24), output24)

    def test_prepare_for_parsing_25(self):
        """prepare_for_parsing Unit test 25: Removing __asm__"""
        input25 = '''int a()__asm__(());'''
        output25 = 'int a();\n'
        self.assertEqual(prepare_for_parsing(input25), output25)

if __name__ == '__main__':
    unittest.main()

