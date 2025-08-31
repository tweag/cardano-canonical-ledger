Reads SCLS file with an arbitrary data inside, generates an output file for each namespace.

Usage:

```
cabal run example-read-file file.scls
```

This file is used to test that data can be compressed and read back.

This file is expected to be removed after we introduce a proper test suite.

Basic manual test for now:

``` sh
cat input.data | cabal run example-write-file file.scls
cabal run example-extract-by-ns file.scls
echo `sha1sum input.data`equal to `sha1sum raw_v0`
```