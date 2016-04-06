# MaxSkins Changelog

## 0.1.5 (2016-04-01)

### Features:

  - Added documentation
  - Added Apache Thrift libraries for Haskell and Erlang
  - Added Haskell and Erlang dependencies in Dockerfile
  - Added e2e_test script for automated testing

### Improvements:

  - Added time breakdown for all examples
  - Improved coding style for MovingAverage, LMemLoopback, PassThrough, SignExt, Simple and VectorAddition examples
  - Added .gitlab-ci.yml linter stage for Cpp, Java, Go and C#
  - Improved .gitlab-ci.yml e2e stage using e2e_test script
  
### Tests:

  - Added e2e test for Sign Extension example

### Examples:

  - Added Basic static, Advanced static and Dynamic interfaces for Correlation example in C#
  - Added Basic static, Advanced static and Dynamic interfaces for Correlation example in Go
  - Added Basic static, Advanced static and Dynamic interfaces for Correlation example in Ruby
  - Added Basic static, Advanced static and Dynamic interfaces for Correlation example in Haskell
  - Added Basic static, Advanced static and Dynamic interfaces for LMemLoopback example in C#
  - Added Basic static, Advanced static and Dynamic interfaces for LMemLoopback example in Go
  - Added Basic static, Advanced static and Dynamic interfaces for LMemLoopback example in Perl
  - Added Basic static and Advanced static interfaces for LMemLoopback example in PHP
  - Added Basic static, Advanced static and Dynamic interfaces for LMemLoopback example in Haskell
  - Added Basic static, Advanced static and Dynamic interfaces for LMemLoopback example in Erlang
  - Added Basic static, Advanced static and Dynamic interfaces for Moving Average example in Haskell
  - Added Basic static, Advanced static and Dynamic interfaces for Moving Average example in Erlang
  - Added Basic static, Advanced static and Dynamic interfaces for Pass Through example in C#
  - Added Basic static, Advanced static and Dynamic interfaces for Pass Through example in Go
  - Added Basic static, Advanced static and Dynamic interfaces for Pass Through example in Perl
  - Added Basic static and Advanced static interfaces for Pass Through example in PHP
  - Added Basic static, Advanced static and Dynamic interfaces for Pass Through example in Haskell
  - Added Basic static, Advanced static and Dynamic interfaces for Pass Through example in Erlang
  - Added Dynamic interface for Sign Extension example in Cpp
  - Added Dynamic interface for Sign Extension example in C#
  - Added Dynamic interface for Sign Extension example in Go
  - Added Dynamic interface for Sign Extension example in Java
  - Added Dynamic interface for Sign Extension example in Ruby
  - Added Dynamic interface for Sign Extension example in Perl
  - Added Dynamic interface for Sign Extension example in Haskell
  - Added Dynamic interface for Sign Extension example in Erlang
  - Added Basic static, Advanced static and Dynamic interfaces for Simple example in C#
  - Added Basic static, Advanced static and Dynamic interfaces for Simple example in Go
  - Added Basic static, Advanced static and Dynamic interfaces for Simple example in Perl
  - Added Basic static and Advanced static interfaces for Simple example in PHP
  - Added Basic static, Advanced static and Dynamic interfaces for Simple example in Haskell
  - Added Basic static, Advanced static and Dynamic interfaces for Simple example in Erlang
  - Added Basic static, Advanced static and Dynamic interfaces for Vector Addition example in C#
  - Added Basic static, Advanced static and Dynamic interfaces for Vector Addition example in Go
  - Added Basic static, Advanced static and Dynamic interfaces for Vector Addition example in Perl
  - Added Basic static and Advanced static interfaces for Vector Addition example in PHP
  - Added Basic static, Advanced static and Dynamic interfaces for Vector Addition example in Haskell
  - Added Basic static, Advanced static and Dynamic interfaces for Vector Addition example in Erlang

## 0.1.4 (2015-10-13)

### Features:

  - Added CHANGELOG.md
  - Added Appache Thrift libraries for C#, Go, PHP and Perl
  - Added Go and Perl dependencies in Dockerfile

### Bugfixes:

  - Updated installation section in README.md (using setup.py for installation)
  - Added transport.close() in LmemLoopback examples
  - Improved coding style in Basic static interface for MovingAverage example in PHP 
  - Improved coding style in Advanced static interface for MovingAverage example in PHP 
  - Improved coding style in Dynamic interface for MovingAverage example in PHP

### Examples:

  - Added Basic static interface for MovingAverage example in Go
  - Added Advanced static interface for MovingAverage example in Go
  - Added Dynamic interface for MovingAverage example in Go
  - Added Basic static interface for MovingAverage example in Perl
  - Added Advanced static interface for MovingAverage example in Perl
  - Added Dynamic interface for MovingAverage example in Perl
  - Added Basic static interface for Correlation example in C++
  - Added Advanced static interface for Correlation example in C++
  - Added Dynamic interface for Correlation example in C++
  - Added Advanced static interface for Correlation example in Python
  - Added Dynamic interface for Correlation example in Python
  - Added Basic static interface for Correlation example in Java
  - Added Advanced static interface for Correlation example in Java
  - Added Dynamic interface for Correlation example in Java

## 0.1.3 (2015-10-06)

### Features:

  - Added h2thrift script
  - Added setup script for installing as a regular python package
  - Added PHP dependencies in Dockerfile
  - Started using setup.py for installation in Dockerfile

### Bugfixes:

  - Updated thrift.jinja and server.jinja templates to support C# and PHP
  - Removed make_server.jinja template
  - Improved build process using fabricate
  - Improved README.md files for examples
  - Improved .pylintrc
  - Improved .gitlab-ci.yml linting and unit-test stages
  - Added .gitlab-ci.yml e2e tests for all examples
  - Added .gitattributes
  - Added exit(-1) in examples after error
  - Added transport.close() in examples

### Examples:

  - Added Basic static interface for MovingAverage example in C#
  - Added Advanced static interface for MovingAverage example in C#
  - Added Dynamic interface for MovingAverage example in C#
  - Added Basic static interface for MovingAverage example in PHP
  - Added Dynamic interface for MovingAverage example in PHP
  - Added Basic static interface for LmemLoopback example in Java
  - Added Advanced static interface for LmemLoopback example in Java
  - Added Dynamic interface for LmemLoopback example in Java
  - Added Basic static interface for PassThrough example in Java
  - Added Advanced static interface for PassThrough example in Java
  - Added Dynamic interface for PassThrough example in Java
  - Added Basic static interface for Simple example in Java
  - Added Advanced static interface for Simple example in Java
  - Added Dynamic interface for Simple example in Java

## 0.1.2 (2015-09-23)

### Features:

  - Upgraded MaxSkins interface
  - Added unit tests
  - Added .gitlab-ci.yml

### Bugfixes:

  - Improved Dockerfile
  - Added .dockerignore
  - Added .pylintrc
  - Updated thrift.jinja and server.jinja templates to support Ruby
  - Improved coding style in Basic static interface for Simple example in C++ 
  - Improved coding style in Advanced static interface for Simple example in C++ 
  - Improved coding style in Dynamic interface for Simple example in C++ 
  - Improved coding style in Basic static interface for Simple example in Python 
  - Improved coding style in Advanced static interface for Simple example in Python 
  - Improved coding style in Dynamic interface for Simple example in Python 
  - Improved coding style in Basic static interface for Simple example in Ruby 
  - Improved coding style in Advanced static interface for Simple example in Ruby 
  - Improved coding style in Dynamic interface for Simple example in Ruby 
  - Improved coding style in Basic static interface for VectorAddition example in Python 
  - Improved coding style in Advanced static interface for VectorAddition example in Python 
  - Improved coding style in Dynamic interface for VectorAddition example in Python 

### Examples:

  - Added Dynamic interface for SignExt example in py
  - Added Basic static interface for LmemLoopback example in C++
  - Added Advanced static interface for LmemLoopback example in C++
  - Added Dynamic interface for LmemLoopback example in C++
  - Added Basic static interface for LmemLoopback example in Python
  - Added Advanced static interface for LmemLoopback example in Python
  - Added Dynamic interface for LmemLoopback example in Python
  - Added Basic static interface for LmemLoopback example in Ruby
  - Added Advanced static interface for LmemLoopback example in Ruby
  - Added Dynamic interface for LmemLoopback example in Ruby
  - Added Basic static interface for PassThrough example in C++
  - Added Advanced static interface for PassThrough example in C++
  - Added Dynamic interface for PassThrough example in C++
  - Added Basic static interface for PassThrough example in Python
  - Added Advanced static interface for PassThrough example in Python
  - Added Dynamic interface for PassThrough example in Python
  - Added Basic static interface for PassThrough example in Ruby
  - Added Advanced static interface for PassThrough example in Ruby
  - Added Dynamic interface for PassThrough example in Ruby

## 0.1.1 (2015-09-17)

### Features:

  - Added LICENSE

## 0.1.0 (2015-09-17)

### Features:

  - Added MaxSkins script
  - Added Dockerfile

### Examples:

  - Added Basic static interface for Correlation example in Python
  - Added Basic static interface for MovingAverage example in C++
  - Added Advanced static interface for MovingAverage example in C++
  - Added Dynamic interface for MovingAverage example in C++
  - Added Basic static interface for MovingAverage example in Python
  - Added Advanced static interface for MovingAverage example in Python
  - Added Dynamic interface for MovingAverage example in Python
  - Added Basic static interface for MovingAverage example in Ruby
  - Added Advanced static interface for MovingAverage example in Ruby
  - Added Dynamic interface for MovingAverage example in Ruby
  - Added Basic static interface for MovingAverage example in Java
  - Added Advanced static interface for MovingAverage example in Java
  - Added Dynamic interface for MovingAverage example in Java
  - Added Basic static interface for Simple example in C++
  - Added Advanced static interface for Simple example in C++
  - Added Dynamic interface for Simple example in C++
  - Added Basic static interface for Simple example in Python
  - Added Advanced static interface for Simple example in Python
  - Added Dynamic interface for Simple example in Python
  - Added Basic static interface for Simple example in Ruby
  - Added Advanced static interface for Simple example in Ruby
  - Added Dynamic interface for Simple example in Ruby
  - Added Basic static interface for VectorAddition example in C++
  - Added Advanced static interface for VectorAddition example in C++
  - Added Dynamic interface for VectorAddition example in C++
  - Added Basic static interface for VectorAddition example in Python
  - Added Advanced static interface for VectorAddition example in Python
  - Added Dynamic interface for VectorAddition example in Python
  - Added Basic static interface for VectorAddition example in Ruby
  - Added Advanced static interface for VectorAddition example in Ruby
  - Added Dynamic interface for VectorAddition example in Ruby
  - Added Basic static interface for VectorAddition example in Java
  - Added Advanced static interface for VectorAddition example in Java
  - Added Dynamic interface for VectorAddition example in Java

