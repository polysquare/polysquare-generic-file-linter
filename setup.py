# /setup.py
#
# Installation and setup script for polysquare-generic-file-linter
#
# See LICENCE.md for Copyright information
"""Installation and setup script for polysquare-generic-file-linter"""

from setuptools import setup, find_packages

setup(name="polysquare-generic-file-linter",
      version="0.0.8",
      description="Polysquare Style Guide Linter",
      long_description="Lint a file for the polysquare style guide",
      author="Sam Spilsbury",
      author_email="smspillaz@gmail.com",
      classifiers=["Development Status :: 3 - Alpha",
                   "Intended Audience :: Developers",
                   "Topic :: Software Development :: Build Tools",
                   "License :: OSI Approved :: MIT License",
                   "Programming Language :: Python :: 3"],
      license="MIT",
      keywords="development linters",
      packages=find_packages(exclude=["tests"]),
      install_requires=["setuptools"],
      extras_require={
          "test": ["coverage",
                   "testtools"]
      },
      entry_points={
          "console_scripts": [
              "polysquare-generic-file-linter=polysquarelinter.linter:main"
          ]
      },
      test_suite="tests")
