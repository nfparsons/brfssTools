test_that(".resolve_states accepts NULL", {
  expect_null(brfssTools:::.resolve_states(NULL))
})

test_that(".resolve_states converts USPS to FIPS", {
  expect_equal(
    brfssTools:::.resolve_states(c("OR", "WA", "CA")),
    c(41L, 53L, 6L)
  )
})

test_that(".resolve_states is case-insensitive on USPS", {
  expect_equal(
    brfssTools:::.resolve_states(c("or", "Wa")),
    c(41L, 53L)
  )
})

test_that(".resolve_states accepts numeric FIPS", {
  expect_equal(
    brfssTools:::.resolve_states(c(41, 53)),
    c(41L, 53L)
  )
})

test_that(".resolve_states errors on unknown USPS", {
  expect_error(
    brfssTools:::.resolve_states(c("OR", "ZZ")),
    "Unknown USPS"
  )
})

test_that(".resolve_states errors on unknown FIPS", {
  expect_error(
    brfssTools:::.resolve_states(c(41L, 99L)),
    "Unknown FIPS"
  )
})

test_that(".resolve_states handles BRFSS territories", {
  # GU=66, PR=72, VI=78
  expect_equal(
    brfssTools:::.resolve_states(c("GU", "PR", "VI")),
    c(66L, 72L, 78L)
  )
})
