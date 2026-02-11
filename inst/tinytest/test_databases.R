# Test retrieve_credentials function

# Skip remaining tests if REDCapR is not installed
if (!requireNamespace("REDCapR", quietly = TRUE)) {
  exit_file("REDCapR not installed, skipping retrieve_credentials tests")
}

# Create a temporary credentials file for testing
cred_file = tempfile(fileext = ".csv")

# Create valid test credentials file
cred_data = data.frame(
  uri = c("https://redcap.example.com/api/", "https://redcap2.example.com/api/"),
  uri_name = c("project_a", "project_b"),
  username = c("user1", "user2"),
  project_id = c(123L, 456L),
  token = c("ABC123TOKEN", "DEF456TOKEN"),
  comment = c("Test project A", "Test project B")
)
write.csv(cred_data, cred_file, row.names = FALSE)

# Test: retrieve by project_id
result = retrieve_credentials(cred_file, project_id = 123L)
expect_true(is.list(result))
expect_equal(result$project_id, 123L)
expect_equal(result$username, "user1")
expect_equal(result$token, "ABC123TOKEN")

# Test: retrieve by uri_name
result = retrieve_credentials(cred_file, uri_name = "project_b")
expect_true(is.list(result))
expect_equal(result$project_id, 456L)
expect_equal(result$username, "user2")

# Test: retrieve by username
result = retrieve_credentials(cred_file, username = "user1")
expect_true(is.list(result))
expect_equal(result$username, "user1")
expect_equal(result$project_id, 123L)

# Test: error when project_id not found
expect_error(retrieve_credentials(cred_file, project_id = 999L))

# Test: error when uri_name not found
expect_error(retrieve_credentials(cred_file, uri_name = "nonexistent"))

# Test: error when username not found
expect_error(retrieve_credentials(cred_file, username = "nobody"))

# Test: invalid credentials file (missing columns)
bad_cred_file = tempfile(fileext = ".csv")
bad_data = data.frame(uri = "https://example.com", token = "ABC123")
write.csv(bad_data, bad_cred_file, row.names = FALSE)

# Suppress warning about missing colClasses columns (expected for malformed file)
expect_error(suppressWarnings(retrieve_credentials(bad_cred_file, project_id = 1L)))

# Clean up tmp files (also auto-cleaned when R session ends)
unlink(cred_file)
unlink(bad_cred_file)