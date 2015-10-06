#!/usr/bin/env python

"""
Maxskins setup scrip.
"""

from setuptools import setup

setup(name='maxskins',
      version='0.1.0',
      description='MaxSkins: DFEs Speak Your Language',
      classifiers=[
          'Development Status :: 3 - Alpha',
          'Programming Language :: Python :: 2.7'
      ],
      url='https://github.com/maxeler/maxskins',
      author='Petar Trifunovic',
      author_email='ptrifunovic@maxeler.com',
      py_modules=[
          'common',
          'parse',
          'format'
      ],
      scripts=[
          'maxskins',
          'h2thrift'
      ],
      install_requires=[
          'docopt==0.6.2',
          'fabricate==1.26',
          'Jinja2'
      ],
      test_suite='nose.collector',
      tests_require=['nose', 'nose-cover3'],
      include_package_data=False,
      packages=['templates'],
      package_data={
          'templates': ['*.jinja'],
      },
      zip_safe=False)
