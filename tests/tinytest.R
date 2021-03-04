
if (requireNamespace("tinytest", quietly = TRUE)) {
  home = identical(Sys.info()["nodename"], "replicant")
  tinytest::test_package("lumisc", at_home = home)
}
