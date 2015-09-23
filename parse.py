"""
Functions for parsing header file into:
    enums list
    functions list
    structs list
    typedefs list

Usage:
    For these functions to work properly
    input code needs to be prepared first,
    function prepare_for_parsing(code)
    has to be called to pre-process the code first.
"""
from format import prepare_for_parsing, remove_duplicates

def parse_enums(code):
    """
    Parses code and returns a list of enums.

    Usage:
        For this function to work properly
        input code needs to be prepared first,
        function prepare_for_parsing(code)
        has to be called to pre-process the code first.

    Input arguments:
        code -- code from which we are parsing enums

    Output:
        List of enums.

        Each enum has:
            name -- name of enum (located between '} ' and ';')
            elements -- list of elements (located between '{' and '}'
                                          and separated by comma)

            Each element has:
                name -- name of the element
                value -- value of the element (only if the
                                               element has value)

    Example:
        >>> code = '''typedef enum mode {
        ...                     NEVER = 0,
        ...                     ALWAYS = 2
        ...           } mode_t;'''
        >>> code = prepare_for_parsing(code)
        >>> print code
        typedef enum mode {NEVER = 0,ALWAYS = 2}mode_t;
        >>> print parse_enums(code)
        [{'elements': [{'name': 'NEVER', 'value': '0'}, {'name': 'ALWAY
        S', 'value': '2'}], 'name': 'mode_t'}]
    """
    def parse_element(element):
        """
        Parses element and returns name and value (optional) of element

        Input arguments:
            element -- element which we are parsing

        Output:
            Dictionary with name and value (if there was one)

        Example:
            >>> element = 'MAX_DEBUG_NEVER = 0'
            >>> print parse_element(element)
            {'name': 'MAX_DEBUG_NEVER', 'value': '0'}
            >>> element = 'MAX_NET_CONNECTION_QSFP_MID_10G_PORT2'
            >>> print parse_element(element)
            {'name': 'MAX_NET_CONNECTION_QSFP_MID_10G_PORT2'}
        """
        if '=' in element:
            return {'name':  element[ : element.find('=')],
                    'value': element[element.find('=') + 1 : ]}
        else:
            return {'name': element}

    return [{'name': line[line.find('}') + 1 : line.find(';')],
             'elements': [parse_element(element)
                          for element
                          in line[line.find('{') + 1 :
                                  line.find('}')].split(',')]}
            for line
            in code.splitlines()
            if 'typedef enum' in line] # line is an enum

def parse_functions(code):
    """
    Parses code and returns a list of functions.

    Usage:
        For this function to work properly
        input code needs to be prepared first,
        function prepare_for_parsing(code)
        has to be called to pre-process the code first.

    Input arguments:
        code -- code from which we are parsing functions

    Output:
        List of functions.

        Each function has:
            type -- type of function (located from beginnig to ' ')
            name -- name of function (located between ' ' and '(')
            arguments -- list of arguments (located between '(' and ')'
                                            and separated by comma)

            Each argument has:
                type -- type of the argument
                name -- name of the argument
                array -- array lenght of argument (optional)

    Example:
        >>> code = '''actarray_t* actarray_init(
        ...                           file_t *file,
        ...                           int      nb_actions);
        ...
        ...           actarray_t* mixed_actarray_init(
        ...                           file_t **files,
        ...                           int nb_actions);
        ...
        ...           typedef enum mode {
        ...                     NEVER = 0,
        ...                     ALWAYS = 2
        ...           } mode_t;'''
        >>> code = prepare_for_parsing(code)
        >>> print code
        actarray_t* actarray_init(file_t* file,int nb_actions);
        actarray_t* mixed_actarray_init(file_t** files,int nb_actions);
        typedef enum mode {NEVER = 0,ALWAYS = 2}mode_t;
        >>> print parse_functions(code)
        [{'type': 'actarray_t*', 'name': 'actarray_init', 'arguments':
        [{'type': 'file_t*', 'name': 'file'}, {'type': 'int', 'name': '
        nb_actions'}]}, {'type': 'actarray_t*', 'name': 'mixed_actarray
        _init', 'arguments': [{'type': 'file_t**', 'name': 'files'}, {'
        type': 'int', 'name': 'nb_actions}]}]
    """
    def parse_arguments(arguments):
        """
        Parses arguments and returns a list of arguments

        Input arguments:
            arguments -- arguments which we are parsing

        Output:
            A list of arguments.

            Each argument has:
                type -- type of the argument
                name -- name of the argument
                array -- array lenght of argument (optional)

        Example:
            >>> arguments = 'file_t* file,int nb_actions'
            >>> print parse_element(element)
            [{'type': 'file_t*', 'name': 'file'}, {'type': 'int', 'name
            ': 'nb_actions'}]
        """
        return [({'type': argument[ : argument.rfind(' ')],
                  'name': argument[argument.rfind(' ') + 1 : ]}
                 if ' ' in argument
                 else {'type': argument})
                if '[' not in argument else
                {'type': argument[ : argument[:argument.find('[')].rfind(' ')],
                 'name': argument[argument[:argument.find('[')].rfind(' ') + 1:
                                  argument.find('[')],
                 'array': argument[argument.find('[') + 1 :
                                   argument.find(']')]}
                for argument
                in arguments.split(',')
                if argument != '']

    # remove same lines from code
    lines = remove_duplicates(code.splitlines())
    return [{'type': line[ : line[ : line.find('(')].rfind(' ')],
             'name': line[line[ : line.find('(')].rfind(' ') + 1
                          : line.find('(')],
             'arguments': parse_arguments(line[line.find('(') + 1 :
                                               line.find(')')])}
            for line
            in lines
            if '(' in line                 #
            if 'typedef' not in line       # line is a function
            if '{' not in line             #
            if 'INIT_NAME' not in line     # INIT_NAME     not supported  TODO
            if 'bindresvport6' not in line # bindresvport6 not supported  TODO
            if 'cb_func' not in line       # cb_func       not supported  TODO
            if '*__func' not in line       # *__func       not supported  TODO
            if '_IO' not in line           # _IO           not supported  TODO
            if '...' not in line]          # ...           not supported  TODO

def parse_structs(code):
    """
    Parses code and returns a list of structures.

    Usage:
        For this function to work properly
        input code needs to be prepared first,
        function prepare_for_parsing(code)
        has to be called to pre-process the code first.

    Input arguments:
        code -- code from which we are parsing structures

    Output:
        List of structures.

        Each structures has:
            name -- name of structure (located between '}' and ';')
            arguments -- list of arguments (located between '{' and '}'
                                            and separated by ';')

            Each argument has:
                type -- type of the argument
                name -- name of the argument
                array -- array lenght of argument (optional)

    Example:
        >>> code = '''actarray_t* mixed_actarray_init(
        ...                           file_t **files,
        ...                           int nb_actions);
        ...           typedef struct event_atomic {
        ...               uint32_t        event_id;
        ...           } event_atomic_t;
        ...
        ...           struct event_startstop {
        ...               uint32_t        event_id;
        ...           };'''
        >>> code = prepare_for_parsing(code)
        >>> print code
        actarray_t* actarray_init(file_t* file,int nb_actions);
        typedef struct event_atomic{uint32_t event_id;}event_atomic_t;
        struct event_startstop{uint32_t event_id;};
        >>> print parse_structs(code)
        [{'name': 'event_atomic_t', 'argument': [{'type': 'uint32_t', '
        name': 'event_id'}]}, {'name': 'event_startstop', 'argument': [
        {'type': 'uint32_t', 'name': 'event_id'}]}]
    """
    def parse_arguments(arguments):
        """
        Parses arguments and returns a list of arguments

        Input arguments:
            arguments -- arguments which we are parsing

        Output:
            A list of arguments.

            Each argument has:
                type -- type of the argument
                name -- name of the argument
                array -- array lenght of argument (optional)

        Example:
            >>> arguments = 'file_t* file;int nb_actions'
            >>> print parse_element(element)
            [{'type': 'file_t*', 'name': 'file'}, {'type': 'int', 'name
            ': 'nb_actions'}]
        """
        return [{'type': argument[ : argument.rfind(' ')],
                 'name': argument[argument.rfind(' ') + 1 : ]}
                if '[' not in argument else
                {'type': argument[ : argument[:argument.find('[')].rfind(' ')],
                 'name': argument[argument[:argument.find('[')].rfind(' ') + 1:
                                  argument.find('[')],
                 'array': argument[argument.find('[') + 1 :
                                   argument.find(']')]}
                for argument
                in arguments.split(';')
                if argument != '']

    return [{'name': line[line.find('}') + 1 : line.rfind(';')]
                     if line[line.find('}') + 1] != ';'
                     else line[: line.find('{')],
             'arguments': parse_arguments(line[line.find('{') + 1 :
                                               line.find('}')])}
            for line
            in code.splitlines()
            if 'struct' in line
            if '{' in line
            if 'union' not in line
            if '}*' not in line]

def parse_typedefs(code):
    """
    Parses code and returns a list of typedefs.

    Usage:
        For this function to work properly
        input code needs to be prepared first,
        function prepare_for_parsing(code)
        has to be called to pre-process the code first.

    Input arguments:
        code -- code from which we are parsing typedefs

    Output:
        List of typedefs.

            Each typedef has:
                type -- type of the typedef
                name -- name of the typedef

    Example:
        >>> code = '''typedef unsigned char u_int8;'''
        >>> code = prepare_for_parsing(code)
        >>> print code
        typedef unsigned char u_int8;
        >>> print parse_typedefs(code)
        [{'type' : 'unsigned char', 'name' : 'u_int8'}]
    """
    return [{'name': line[line.rfind(' ') + 1 : line.find(';')],
             'type': line[line.find(' ') + 1 : line.rfind(' ')]}
            for line
            in code.splitlines()
            if 'typedef' in line
            if '(' not in line
            if '{' not in line
            if '[' not in line]

def parse(code):
    """
    Parses code and returns functions, enums, structures and typedefs.

    Usage:
        For this function to work properly
        input code needs to be preprocessed first,
        function cpp -I /opt/maxcompiler/include/slic code new_code
        has to be called to pre-process the code first.

    Input arguments:
        code -- code which we are parsing

    Output:
        Dictionary with functions, enums, structs and typedefs.

        functions -- list of functions
        Each function has:
            type -- type of function (located from beginnig to ' ')
            name -- name of function (located between ' ' and '(')
            arguments -- list of arguments (located between '(' and ')'
                                            and separated by comma)

            Each argument has:
                type -- type of the argument
                name -- name of the argument
                array -- array lenght of argument (optional)

        enums -- list of enums
        Each enum has:
            name -- name of enum (located between '} ' and ';')
            elements -- list of elements (located between '{' and '}'
                                          and separated by comma)

            Each element has:
                name -- name of the element
                value -- value of the element (only if the
                                               element has value)

        structs -- list of structures
        Each structure has:
            name -- name of structure (located between '}' and ';')
            arguments -- list of arguments (located between '{' and '}'
                                            and separated by ';')

            Each argument has:
                type -- type of the argument
                name -- name of the argument
                array -- array lenght of argument (optional)

        typedefs -- list of typedefs.
        Each typedef has:
            type -- type of the typedef (located between first ' '
                                         and last ' ')
            name -- name of the typedef (located between last ' '
                                         and ';')

    Example:
        >>> code = '''typedef unsigned char u_int8;
        ...
        ...           max_actarray_t* max_aaaaaaactarray_init(
        ...                           max_file_t *maxfile,
        ...                           int      nb_actions);
        ...
        ...           max_actarray_t* max_mixed_actarray_init(
        ...                           max_file_t **maxfiles,
        ...                           int nb_actions);
        ...
        ...           typedef enum max_debug_mode {
        ...                      MAX_DEBUG_NEVER = 0,
        ...                      MAX_DEBUG_ON_ERROR = 1,
        ...                      MAX_DEBUG_ALWAYS = 2
        ...           } max_debug_mode_t;
        ...
        ...           typedef struct max_event_atomic {
        ...               uint32_t        event_id;
        ...           } max_event_atomic_t;
        ...
        ...           typedef struct max_event_startstop {
        ...               uint32_t        event_id;
        ...           } max_event_startstop_t;'''
        >>> code = prepare_for_parsing(code)
        >>> print parse(code)
        {'functions': [{'type': 'max_actarray_t*', 'name': 'max_aaaaaaa
        ctarray_init', 'arguments': [{'type': 'max_file_t*', 'name': 'm
        axfile'}, {'type': 'int', 'name': 'nb_actions'}]}, {'type': 'ma
        x_actarray_t*', 'name': 'max_mixed_actarray_init', 'arguments':
         [{'type': 'max_file_t**', 'name': 'maxfiles'}, {'type': 'int',
         'name': 'nb_actions'}]}], 'enums': [{'elements': [{'name': 'MA
        X_DEBUG_NEVER', 'value': '0'}, {'name': 'MAX_DEBUG_ON_ERROR', '
        value': '1'}, {'name': 'MAX_DEBUG_ALWAYS', 'value': '2'}], 'nam
        e': 'max_debug_mode_t'}], 'structs': [{'name': 'max_event_atomi
        c_t', 'argument': [{'type': 'uint32_t', 'name': 'event_id'}]},
        {'name': 'max_event_startstop_t', 'argument': [{'type': 'uint32
        _t', 'name': 'event_id'}]}], 'typedefs': [{'type' : 'unsigned c
        har', 'name' : 'u_int8'}]}
    """
    code = prepare_for_parsing(code)
    return {'enums': parse_enums(code),
            'functions': parse_functions(code),
            'structs': parse_structs(code),
            'typedefs': parse_typedefs(code)}

