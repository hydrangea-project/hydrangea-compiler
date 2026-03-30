{
  description = "Hydrangea development environment and build outputs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        hasAttr = builtins.hasAttr;
        source = lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let
              base = builtins.baseNameOf path;
              pathStr = toString path;
            in
              !(lib.elem base [
                ".git"
                ".direnv"
                "dist-newstyle"
                "result"
                "hydrangea_out"
                "hydrangea_out.c"
              ])
              && !(lib.hasSuffix ".agdai" pathStr)
              && !(lib.hasSuffix ".aux" pathStr)
              && !(lib.hasSuffix ".glob" pathStr)
              && !(lib.hasSuffix ".log" pathStr)
              && !(lib.hasSuffix ".out" pathStr)
              && !(lib.hasSuffix ".pdf" pathStr)
              && !(lib.hasSuffix ".toc" pathStr)
              && !(lib.hasSuffix ".vo" pathStr)
              && !(lib.hasSuffix ".vok" pathStr)
              && !(lib.hasSuffix ".vos" pathStr);
        };
        openmpCompiler = pkgs.gcc;
        solverTools = [ pkgs.z3 ];
        hydrangea = pkgs.haskellPackages.callCabal2nix "hydrangea" source { };
        hydrangeaCompiler = pkgs.symlinkJoin {
          name = "hydrangea-compiler";
          paths = [ hydrangea ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram "$out/bin/hydrangea-compiler" \
              --prefix PATH : ${lib.makeBinPath ([ openmpCompiler ] ++ solverTools)} \
              --set-default CC ${openmpCompiler}/bin/gcc
          '';
        };
      in {
        packages = {
          default = hydrangeaCompiler;
          hydrangea-compiler = hydrangeaCompiler;
        };

        apps = {
          default = {
            type = "app";
            program = "${hydrangeaCompiler}/bin/hydrangea-compiler";
          };
          hydrangea-compiler = {
            type = "app";
            program = "${hydrangeaCompiler}/bin/hydrangea-compiler";
          };
        };

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.cabal-install
            pkgs.gnumake
            pkgs.haskellPackages.alex
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.happy
            openmpCompiler
            pkgs.z3
          ];
          shellHook = ''
            export CC=${openmpCompiler}/bin/gcc
            echo "Hydrangea dev shell ready (CC=$CC)"
          '';
        };
      });
}
