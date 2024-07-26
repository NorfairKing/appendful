{ lib
, haskell
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: _:
let
  appendfulPkg = name: doBenchmark (buildStrictly (self.callPackage (../${name}) { }));
  appendfulPackages = {
    appendful = appendfulPkg "appendful";
    genvalidity-appendful = appendfulPkg "genvalidity-appendful";
    appendful-persistent = appendfulPkg "appendful-persistent";
  };
in
{
  inherit appendfulPackages;

  appendfulRelease =
    symlinkJoin {
      name = "appendful-release";
      paths = attrValues self.appendfulPackages;
    };
} // appendfulPackages
