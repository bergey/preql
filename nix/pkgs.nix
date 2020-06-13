{
  extras = hackage:
    { packages = {
        preql = ./preql.nix;
        stm = hackage.stm."2.5.0.0".revisions.default;
      }; };
  resolver = "lts-15.4";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }
