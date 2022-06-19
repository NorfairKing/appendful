final: previous:
with final.lib;
with final.haskell.lib;
let
  appendfulPkg = name:
    doBenchmark (
      buildStrictly (
        final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
      )
    );
in
{
  appendfulPackages =
    {
      appendful = appendfulPkg "appendful";
      genvalidity-appendful = appendfulPkg "genvalidity-appendful";
      appendful-persistent = appendfulPkg "appendful-persistent";
    };
  appendfulRelease =
    final.symlinkJoin {
      name = "appendful-release";
      paths = attrValues final.appendfulPackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: { })) (
            self: super: final.appendfulPackages
          );
      }
    );
}
