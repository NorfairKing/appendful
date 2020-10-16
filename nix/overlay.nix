final: previous:
with final.haskell.lib;
let
  appendfulPkg = name:
    doBenchmark (
      failOnAllWarnings (
        final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
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
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super: final.appendfulPackages
            );
        }
    );
}
