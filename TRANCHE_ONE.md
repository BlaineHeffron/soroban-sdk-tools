# Video for SCF tranch one

[Watch video here](https://drive.google.com/file/d/1NPAQjP4UTPAxC5rxTz3n-q3DdaH01VA3/view?usp=sharing)

Added a `contractstorage` macro and supporting Map and Item types.

Added three key generation options:
1. Symbolic, similar to the current `DataKey` method used by most contracts and produces human readable keys, although uses more data
2. Short keys, similar to symbolic but allows using a custom smaller prefix
3. Hash keys for keys which are more than 32 bytes this option lets you ensure a fixed length while maintaining unique keys.
﻿
The macro also has an auto-shorten feature which hash by default otherwise let you override with `symbolic`  which will find the smallest unique short key.
