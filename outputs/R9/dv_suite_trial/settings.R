# Settings used by every step. Change a convention here, once, not per DV.

settings <- list(
  # Household identifier: groups people into households for the household totals and flags.
  # Person-level DVs are worked out row by row and do not use it.
  by = "hhserial",

  # Count a missing value as zero when adding up (decision D3)
  na_as_zero = TRUE,

  # Sentinel codes dv_clear_sentinels() replaces, and the value put in their place (decision D1)
  codes = c(-8, -9),
  replacement = 0,

  # What a DV holds where its input is -8/-9 (decision D1): "zero" puts 0, "missing" puts NA,
  # "stop" refuses to run. Copies, band midpoints, flags, kept values and totals use this.
  # The published Round 8 DVs hold 0 there. A step can override it, e.g. sentinels = "keep"
  # in a dv_copy() whose published DV keeps -9.
  sentinels = "zero",

  # The same for differences, ratios and present values. The published Round 8 DVs leave
  # these missing where an input is -8/-9.
  sentinels_in_calculations = "missing",

  # Pension discount rate from the pension rates lookup. Pension DVs stop until this is set.
  rate = NULL,

  # Age pension amounts are expected at
  target_age = 66
)
