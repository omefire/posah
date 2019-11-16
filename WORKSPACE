workspace(name = "prunel")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    sha256 = "40fd6de12324b515042634ba13b02fa19f5c6e274eae6350be2e4d1e023fcd90",
    strip_prefix = "rules_haskell-0.11",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.11.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

# Import and load the Bazel rules to build Nix packages.
http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-33c50ba64c11dddb95823d12f6b1324083cc5c43",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/33c50ba64c11dddb95823d12f6b1324083cc5c43.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_package(name = "ghc")