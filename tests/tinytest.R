
if (requireNamespace("tinytest", quietly = TRUE)) {
  home = Sys.info()["nodename"] == "replicant"
  tinytest::test_package("lumisc", at_home = home)
}
