# Loaded before the example tests: makes the suite available as `suite`.
library(autospec)
options(autospec.quiet = TRUE)
suite <- suppressMessages(read_dv_suite(".."))
