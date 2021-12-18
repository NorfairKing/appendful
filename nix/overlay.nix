final: previous:
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
