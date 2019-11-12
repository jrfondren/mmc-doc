# mmc-doc
Look up Mercury documentation.

## usage
```
$ mmc-doc io         # view 'io' module documentation
$ mmc-doc -l io      # jump to 'io' section of unified stdlib docs
$ mmc-doc -i ref     # open Mercury Language Reference Manual

$ mmc-doc io print_line  # search io interface for 'print_line' as a token
$ mmc-doc io -g print    # search for 'print' as a substring
$ mmc-doc io -c print    # search for anything with 'print' in its comments
$ mmc-doc io error -t type  # search io 'type' declarations for 'error'
```

## build
```
make
```

## install
```
make install
```
This installs 'mmc-doc' into ~/bin

## configuration
to view HTML docs, $WWWPAGER, then 'which w3m', then 'which xdg-open' is tried.

cache files are placed in ($XDG_CONFIG_HOME || $APPDATA || $HOME/config)/mmc-doc/

the directory that installed Mercury documentation can be found at is
calculated from 'which mmc', and can be overridden by an $MMDOC_HTMLDIR
environment variable.

documentation is fetched from mercurylang.org current ROTD docs by default, or
from a mercury-in.space backup if the '-b' flag is provided.

## caveats
- since interfaces aren't really parsed, queries like 'array::array_di' (to
  find any rule where an array is destructively input), these aren't possible,
  as the type and mode are often in separate declarations.
- depends on wget to fetch cached documentation.
- despite any suggestions from name 'mmc-doc', it is not an official part of Mercury.
