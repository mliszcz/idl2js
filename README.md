# idl2js

Quick and dirty CORBA IDL to ECMAScript translator / interfaces generator.

Usage:
```bash
sbt compile pack
./target/pack/bin/main ./idl/tango.idl > ./generated.js
```
