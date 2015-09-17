import unittest
import sys

sys.path.append('..')
from format import *

class TestFormat(unittest.TestCase):

    def setUp(self):
        pass

    # Tests for remove_duplicates_from_list(seq)
    def test_remove_duplicates_from_list_1(self):
        input = [1, 2, 2, 6, 4, 6, 1]
        output = [1, 2, 6, 4]
        self.assertEqual(remove_duplicates_from_list(input), output)

    def test_remove_duplicates_from_list_2(self):
        input = ['a', 'b', 'c', 'c']
        output = ['a', 'b', 'c']
        self.assertEqual(remove_duplicates_from_list(input), output)

    # Tests for transform(code, transformations)
    def test_transform_1(self):
        input = 'This is a test'
        transformations = [['is', 'was'], ['test', 'test!']]
        output = 'Thwas was a test!'
        self.assertEqual(transform(input, transformations), output)

    def test_transform_2(self):
        input = 'a   b nna'
        transformations = [['a', 'b'], ['  ', ' ']]
        output = 'b  b nnb'
        self.assertEqual(transform(input, transformations), output)

    # Tests for prepare_for_parsing(code)
    def test_prepare_for_parsing_1(self):
        input = "hi    bye"
        output = "hi bye"
        self.assertEqual(prepare_for_parsing(input), output)

    def test_prepare_for_parsing_2(self):
        input = "            "
        output = ""
        self.assertEqual(prepare_for_parsing(input), output)

    def test_prepare_for_parsing_3(self):
        input = "        df **  fl** "
        output = "df** fl**"
        self.assertEqual(prepare_for_parsing(input), output)

if __name__ == '__main__':
    unittest.main()
