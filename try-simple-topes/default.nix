with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) {});
let simple-topes-local = pkgs.haskell.packages.ghcjs.callPackage ../simple-topes/simple-topes.nix { };
in pkgs.haskell.packages.ghcjs.callCabal2nix "try-simple-topes" ./. { simple-topes = simple-topes-local; }
