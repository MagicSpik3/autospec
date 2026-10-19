#' Which suite steps to export, and why each of the others is left out
#'
#' A step with no function body is never exported. With `finished_only`, a step
#' is exported only when its plan row is finished (finished_dvs()) and every
#' suite DV it reads is exported too.
#'
#' @param steps The suite's steps, named by DV.
#' @param plan The plan, needed when `finished_only` is `TRUE`.
#' @param finished_only Whether to export only finished DVs.
#' @return A list: `keep`, a logical per step, and `left_out`, a data frame with
#'   `dv`, `uid` and `reason` for each step not kept.
#' @keywords internal
#' @noRd
select_export_steps <- function(steps, plan = NULL, finished_only = FALSE) {
  dvs <- names(steps)
  uids <- step_uids(steps)
  written <- vapply(steps, function(step) is.function(step$derive), logical(1L), USE.NAMES = FALSE)

  reason <- rep(NA_character_, length(steps))
  reason[!written] <- "not written"

  if (finished_only) {
    position <- match_uid_or_dv(uids, dvs, plan$uid, plan$dv)
    plan_reason <- ifelse(is.na(position), "not in the plan", tracking_gap(plan)[position])
    reason[is.na(reason)] <- plan_reason[is.na(reason)]
    reason <- leave_out_dependants(steps, reason)
  }

  left <- !is.na(reason)

  list(
    keep = !left,
    left_out = data.frame(dv = dvs[left], uid = uids[left], reason = reason[left], stringsAsFactors = FALSE)
  )
}


#' Leave out each kept step that reads a step left out, until none do
#' @keywords internal
#' @noRd
leave_out_dependants <- function(steps, reason) {
  dvs <- tolower(names(steps))

  repeat {
    left_out <- dvs[!is.na(reason)]
    needs <- lapply(steps, function(step) intersect(tolower(step$inputs), left_out))
    blocked <- is.na(reason) & lengths(needs) > 0L

    if (!any(blocked)) {
      return(reason)
    }

    reason[blocked] <- vapply(needs[blocked], function(needed) {
      names <- names(steps)[match(needed, dvs)]
      paste0("needs ", paste(names, collapse = ", "), ", which ", if (length(names) == 1L) "is" else "are", " left out")
    }, character(1L))
  }
}
