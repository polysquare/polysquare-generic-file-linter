Polysquare Generic File Linter
==============================

Checks each file passed in for compliance with polysquare style guidelines.

* `headerblock/filename`: Checks that the first line of the file has a line
                          which matches `/path/to/file` from the source root
* `headerblock/desc_space`: Checks that the second line of the file is an
                            empty comment
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