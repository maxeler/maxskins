# MaxSkins: DFEs Speak Your Language

**DFEs can now be taught to speak almost any language.**

MaxSkins generates Appache Thrift wrappers for SLiC interfaces (basic static, advanced static and dynamic) for various programming languages.

# Installation

## SW Requirements

- MaxCompiler
- Apache Thrift (instructions on how to install Apache Thrift: <https://thrift.apache.org/docs/install>)

## Step 1. Install all necessary Python requirements

```bash
pip install -r requirements.txt
```
    
## Step 2. Add maxskins utility to the PATH

```bash
PATH = <maxskins_dir>:$PATH
```

# Quick Start

It is recomended to use Docker image with all pre-installed dependencies. Find more about Docker at <https://www.docker.com>.

## How to build MaxSkins Docker image?

### Requirements

- Maxcompiler 201x.x installer '.tar.gz' (must be copied to the root dir of the project)

### Command to build MaxSkins Docker image

```bash
docker build -t maxskins .
```

## How to run MaxSkins Docker container?

### Command to run bash shell in maxskins container

```bash
docker run -ti --rm maxskins /bin/bash
```
    
### Command to run shell with directory mounted from the host

```bash
docker run -ti --rm -v <path_of_host_dir>:<path_of_docker_dir> maxskins /bin/bash
```

Find more about volumes at <https://docs.docker.com/userguide/dockervolumes>.

```bash
Usage:
    maxskins [-t <target>] <maxfile> [-d <output_directory>]
    maxskins -h | --help
    maxskins -v | --version
    
Options:
    -t <target>             Targets: Python (py), Java (java), C++ (cpp), Ruby (rb), Go (go), C# (csharp), Haskell (hs) [default: cpp] 
    -d <output_directory>   Output directory [default = .]
    -h --help               Show this message
    -v --version            Show Maxskins version
```

## Examples

The examples for the maxskins utility usage are located in the **examples** directory.
For each example there is a README.md file. All .max files are pre-compiled with the MaxCompiler v2014.2.