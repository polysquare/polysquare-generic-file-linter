Polysquare Generic File Linter
==============================

Status
------

| Travis CI | AppVeyor | Coverage | PyPI |
|-----------|----------|----------|------|
|[![Travis](https://travis-ci.org/polysquare/polysquare-generic-file-linter.svg?branch=master)](https://travis-ci.org/polysquare/polysquare-generic-file-linter)|[![AppVeyor](https://ci.appveyor.com/api/projects/status/100s4jga4wc92lq0/branch/master?svg=true)](https://ci.appveyor.com/project/smspillaz/polysquare-generic-file-linter/branch/master)|[![Coverage](https://coveralls.io/repos/polysquare/polysquare-generic-file-linter/badge.png?branch=master)](https://coveralls.io/r/polysquare/polysquare-generic-file-linter?branch=master)|[![PyPI](https://pypip.in/version/polysquare-generic-file-linter/badge.svg)](https://pypi.python.org/pypi/polysquare-generic-file-linter/)|

Checks each file passed in for compliance with polysquare style guidelines.

* `headerblock/filename`: Checks that the first line of the file has a line
                          which matches `/path/to/file` from the source root
* `headerblock/desc_space`: Checks that the second line of the headerblock is
                            as empty comment
* `headerblock/space_copyright`: Checks that the second last line of the
                                 headerblock is an empty comment
* `headerblock/copyright`: Checks that the last line of the headerblock
                           contains an appropriate short-form copyright
                           notice
* `file/newline_last_char`: Checks that the last line is just a \n

Usage
-----

    usage: polysquare-generic-file-linter [-h] [--checks]
                                          [--whitelist [WHITELIST ...]
                                          [--blacklist [BLACKLIST ...]
                                          FILE

    Lint for Polysquare style guide

    positional arguments:
      FILE                  read FILE

    optional arguments:
      -h, --help            show this help message and exit
      --checks              list available checks
      --whitelist [WHITELIST [WHITELIST ...]]
                        list of checks that should only be run
      --blacklist [BLACKLIST [BLACKLIST ...]]
                        list of checks that should never be run
