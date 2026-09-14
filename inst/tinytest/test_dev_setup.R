tmp = tempfile()
on.exit(unlink(tmp, recursive = TRUE))

nested = file.path(tmp, "a", "b", "c")
create_dir(nested)
expect_true(dir.exists(nested))

create_dir(nested)
expect_true(dir.exists(nested))

rproj_path = file.path(tmp, "test.Rproj")
create_rproj(rproj_path)
expect_true(file.exists(rproj_path))

create_rproj(rproj_path)
expect_false(create_rproj(rproj_path))

rproj_no_ext = file.path(tmp, "test2")
create_rproj(rproj_no_ext)
expect_true(file.exists(file.path(tmp, "test2.Rproj")))

lintr_path = file.path(tmp, ".lintr")
create_lintr(tmp)
expect_true(file.exists(lintr_path))

create_lintr(tmp)
expect_false(create_lintr(tmp))

air_path = file.path(tmp, "air.toml")
create_air(tmp)
expect_true(file.exists(air_path))

create_air(tmp)
expect_false(create_air(tmp))

lines = readLines(air_path)
expect_true(any(grepl("line-width", lines)))
expect_true(any(grepl("indent-width", lines)))