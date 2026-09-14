cred_file = tempfile(fileext = ".csv")

cred_data = data.frame(
  uri = c("https://redcap.example.com/api/", "https://10.0.0.5/api/"),
  uri_name = c("project_a", "project_b"),
  username = c("user1", "user2"),
  project_id = c(123L, 456L),
  token = c("0123TOKEN", "DEF456TOKEN"),
  comment = c("Luke's project", "Test, project B")
)
write.csv(cred_data, cred_file, row.names = FALSE)
write("# a comment line", cred_file, append = TRUE)

result = retrieve_credentials(cred_file, project_id = 123L)
expect_true(is.list(result))
expect_equal(result$project_id, "123")
expect_equal(result$username, "user1")
expect_equal(result$token, "0123TOKEN")
expect_equal(result$comment, "Luke's project")

result = retrieve_credentials(cred_file, uri_name = "project_b", check_url = TRUE)
expect_equal(result$project_id, "456")
expect_equal(result$comment, "Test, project B")

expect_equal(retrieve_credentials(cred_file, username = "user1", project_id = "123")$uri_name, "project_a")

expect_error(retrieve_credentials(cred_file, project_id = 999L), class = "credential_match_error")
expect_error(retrieve_credentials(cred_file, uri_name = "project_a", username = "user2"), class = "credential_match_error")
expect_error(retrieve_credentials(cred_file), class = "credential_match_error")
expect_error(retrieve_credentials(cred_file, 123L), class = "invalid_filter_error")
expect_error(retrieve_credentials(cred_file, team = "a"), class = "invalid_credentials_error")

bad_cred_file = tempfile(fileext = ".csv")
write.csv(data.frame(uri = "https://example.com", user = "a"), bad_cred_file, row.names = FALSE)
expect_error(retrieve_credentials(bad_cred_file, user = "a"), class = "invalid_credentials_error")

expect_true(lumisc:::is_valid_url("https://example.com"))
expect_true(lumisc:::is_valid_url("http://example.com"))
expect_true(lumisc:::is_valid_url("https://sub.example.com/path"))
expect_true(lumisc:::is_valid_url("ftp://files.example.com"))
expect_true(lumisc:::is_valid_url("https://10.0.0.5:8443/api"))
expect_true(lumisc:::is_valid_url("http://localhost:8080"))
expect_false(lumisc:::is_valid_url("not_a_url"))
expect_false(lumisc:::is_valid_url(""))

unlink(c(cred_file, bad_cred_file))
