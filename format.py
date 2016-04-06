"""
Functions used for formating header file.
"""
def remove_duplicates(seq):
    """
    Removes duplicates from a list.

    This is the fastest solution, source:
        http://www.peterbe.com/plog/uniqifiers-benchmark

    Input arguments:
        seq -- list from which we are removing duplicates

    Output:
        List without duplicates.

    Example:
        >>> seq = ['a', 'a', 'b', 'c', 'a']
        >>> print remove_duplicates_from_list(seq)
        ['a', 'b', 'c']
    """
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]

def transform(text, transformations):
    """
    Replaces every occurrenc of transformation[0] with transformation[1]
    for every transformation in transformations.

    Input arguments:
        text -- text we are transforming
        transformations -- list of transformations
            Each transformation has:
                transformation[0] -- expresion to be transformed
                transformation[1] -- expresion to which it transforms

    Output:
        Transformed text.

    Example:
        >>> text = '''This is a test'''
        >>> transformations = [['is', 'was'], ['test', 'test!']]
        >>> print transform(text, transformations)
        Thwas was a test!
    """
    for transformation in transformations:
        text = text.replace(transformation[0], transformation[1])
    return text

def prepare_for_parsing(code):
    """
    Removes all c style comments and prepares code for parsing.

    Usage:
        For this function to work properly
        input code needs to be preprocessed first,
        function g++ -E -I /opt/maxcompiler/include/slic code new_code
        has to be called to preprocess the code first.

    Input arguments:
        code -- code we are preparing for parsing

    Output:
        Returns code with no:
            c style comments,
            includes,
            defines,
            externs,
            macros
        It also format the code so that every line is either:
            function
            enum
            struct
            typedef

    Example:
        >>> code = '''actarray_t* actarray_init(
        ...                           max_file_t *file,
        ...                           int      nb_actions);
        ...
        ...           actarray_t* mixed_actarray_init(
        ...                           file_t **files,
        ...                           int nb_actions);
        ...           typedef enum debug_mode {
        ...                      NEVER = 0,
        ...                      ALWAYS = 2
        ...           } debug_mode_t;'''
        >>> print prepare_for_parsing(code)
        actarray_t* actarray_init(file_t* file,int nb_actions);
        actarray_t* mixed_actarray_init(file_t** files,int nb_actions);
        typedef enum debug_mode{NEVER = 0,ALWAYS = 2}max_debug_mode_t;
    """
    # remove empty lines and comments
    code = '\n'.join([line for line in code.splitlines()
                      if '#' not in line
                      if line != ''])

    # remove excess spaces
    code = ' '.join(code.split())

    code = transform(code, [['; ', ';\n'], # all lines end with ;
                            # move * next to the type int *a --> int* a
                            [' ***', '*** '], [' **', '** '], [' *', '* '],
                            # remove spaces around comma and parentheses
                            [', ', ','], [' ,', ','], ['; ', ';'], [' ;', ';'],
                            ['( ', '('], [' (', '('], [') ', ')'], [' )', ')'],
                            ['{ ', '{'], [' {', '{'], ['} ', '}'], [' }', '}'],
                            ['= ', '='], [' =', '='], [' [', '['],
                            # remove extern, __restrict, __signed__,
                            # __extension__ __inline and volatile
                            ['extern ', ''], ['__restrict', ''],
                            ['__signed__ ', ''], ['__extension__ ', ''],
                            ['__inline ', ''], ['volatile ', ''],
                            # remove void if it's only arguments
                            ['(void)', '()'],
                            # remove double spaces, and spaces before [
                            ['  ', ' '], ['[]', '[ ]']])

    # puts structures in one line
    # and removes __asm__((...)), __attribute__((...)) and __WAIT_STATUS((...))
    new_code = ''
    in_struct = 0
    for line in code.splitlines():
        in_struct += line.count('{') - line.count('}')
        if in_struct < 0:
            in_struct = 0
        if in_struct > 0:
            new_code += line
        else:
            if '__attribute__' in line or '__asm__' in line:
                cut = (line.find('__attribute__')
                       if line.find('__attribute__') != -1
                       else len(line))
                cut = (line.find('__asm__')
                       if line.find('__asm__') < cut
                       and line.find('__asm__') != -1
                       else cut)
                cut = (line.find('__WAIT_STATUS')
                       if line.find('__WAIT_STATUS') < cut
                       and line.find('__WAIT_STATUS') != -1
                       else cut)
                new_code += line[:cut] + line[line.rfind('))') + 2:]
            else:
                new_code += line
            new_code += ('\n'
                         if ';' in line
                         else '')

    return transform(new_code, [[' ;', ';']])

