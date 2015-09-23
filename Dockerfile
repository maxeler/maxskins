FROM centos:7
MAINTAINER Petar Trifunovic <ptrifunovic@maxeler.com>

##########################
# Installing MaxCompiler #
##########################
RUN yum -y groupinstall "Development Tools"

RUN yum -y install lapack bla

RUN yum -y update

RUN yum install -y wget ant

ADD maxcompiler-*-installer.tar.gz /
RUN cd maxcompiler-*-full-installer && echo yes | ./install --edition s /opt/maxcompiler

ENV MAXCOMPILERDIR=/opt/maxcompiler/
ENV PATH=/opt/maxcompiler/bin:$PATH
ENV MAXELEROSDIR=$MAXCOMPILERDIR/lib/maxeleros-sim
ENV LD_PRELOAD=$MAXELEROSDIR/lib/libmaxeleros.so

############################
# Installing Apache Thrift #
############################
# Add EPEL repository
RUN yum -y install epel-release

# General dependencies
RUN yum -y install libevent-devel zlib-devel openssl-devel python-devel bzip2-devel boost-devel net-tools

# Java dependencies
RUN yum -y install junit ant-junit

# Python dependencies
RUN yum -y install python-pip

# Ruby dependencies
RUN yum install -y ruby ruby-devel rubygems && \
    gem install bundler rake

# C# dependencies
RUN yum install -y mono-core mono-devel mono-web-devel mono-extras mingw32-binutils mingw32-runtime mingw32-nsis

# Install Thrift
RUN yum -y install thrift.x86_64 thrift-devel.x86_64

# Install Thrift libraries for various languages
RUN yum -y install libthrift-java.noarch libthrift-javadoc.noarch python-thrift.x86_64
RUN gem install thrift

############################
#   Installing MaxSkins    #
############################
ADD . /opt/maxskins

RUN cd /opt/maxskins &&\
    pip install -r requirements.txt

ENV PATH=/opt/maxskins/:$PATH

WORKDIR /opt/maxskins
