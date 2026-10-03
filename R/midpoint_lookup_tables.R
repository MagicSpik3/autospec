# Midpoint lookup tables -----------------------------------------------

gcontvb_lookup <- c(
  `1` = 2500,
  `2` = 7500,
  `3` = 15000,
  `4` = 25000,
  `5` = 35000,
  `6` = 45000,
  `7` = 62500,
  `8` = 87500,
  `9` = 150000,
  `10` = 300000
)

ugdvbs_lookup <- c(
  `1` = 0,
  `2` = 2500,
  `3` = 7500,
  `4` = 15000,
  `5` = 25000,
  `6` = 35000,
  `7` = 45000,
  `8` = 62500,
  `9` = 87500,
  `10` = 150000,
  `11` = 300000
)

# Same coding for UGdVBL and UGdVBOS
ugdvbl_lookup  <- ugdvbs_lookup
ugdvbos_lookup <- ugdvbs_lookup


# Helper function ------------------------------------------------------

apply_midpoint <- function(x, lookup) {
  unname(lookup[as.character(x)])
}
