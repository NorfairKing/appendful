{ sources ? ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
  pkgs = pkgsv { };
in
pkgsv {
  overlays =
    [
      (import (sources.validity + "/nix/overlay.nix"))
      (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
      (import ./overlay.nix)
    ];
}
