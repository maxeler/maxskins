# Examples

For each example there is a README.md file.
All .max files are pre-compiled with the MaxCompiler v2014.2.

## Usage

Before running examples some environmental variables have to be set.

### Before running C# examples

```bash
export MONO_PATH=<maxskins_dir>/examples/lib/csharp/
```

### Before running Go examples

```bash
export GOPATH=<maxskins_dir>/examples/lib/go
```

### Before running Perl examples

```bash
export PERL5LIB=<maxskins_dir>/examples/lib/perl/
```

### Before running PHP examples

```bash
export PHP_THRIFT_LIB=<maxskins_dir>/examples/lib/php
```

### Before running HASKELL examples

```bash
export HASKELLPATH=../gen-hs/:<maxskins_dir>/examples/lib/hs
```

### Before running ERLANG examples

```bash
export EBINPATH=/opt/maxskins/examples/lib/erl/ebin/
export EINCLUDEPATH=/opt/maxskins/examples/lib/erl/include/
mkdir $EBINPATH && \
erlc -I $EINCLUDEPATH -o $EBINPATH /opt/maxskins/examples/lib/erl/src/*.erl
```
