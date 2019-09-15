# mmc-doc
Look up Mercury documentation.

## usage
```
$ mmc-doc io         # view 'io' module documentation
$ mmc-doc -l io      # jump to 'io' section of unified stdlib docs
$ mmc-doc -i ref     # open Mercury Language Reference Manual
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

## caveats
- depends on w3m for viewing documentation.
- depends on wget to fetch cached documentation.
- despite any suggestions from name 'mmc-doc', it is not an official part of Mercury.
