#' DSWhere
#'
#' Temporary row condition context
#'
#' Provides row-scoped operations for a `DS` object using a condition
#' captured by `$where()`. Intended for immediate use in a single expression.
#'
#' @name DSWhere
#' @noRd
DSWhere = R6::R6Class("DSWhere", private = list(
		ds = NULL,
		condition = NULL
))



# Initializes a temporary context with its parent dataset and row condition.
DSWhere$set("public", "initialize", function(ds, condition) {
	private$ds = ds
	private$condition = condition

	invisible(NULL)
})




# Returns the number of rows currently matching the condition.
DSWhere$set("active", "nrow", \() sum(eval_row_mask(private$condition, private$ds$data)))

# DSWhere$set("active", "mask", \() eval_row_mask(private$condition, private$ds$data))
# DSWhere$set("active", "nrow", \() sum(self$mask))
# DSWhere$set("active", "nrow", \() sum(private$mask))
# DSWhere$set("active", "prop", \() mean(private$mask))
# DSWhere$set("active", "pct", \() private$prop * 100)




# consider
# ds$where(condition)$set(...)
# ds$where(condition)$clear(...)
# ds$where(condition)$add(...)
# ds$where(condition)$discard(...)
# ds$where(condition)$strip(...)
# ds$where(condition)$recode(...)
# ds$where(condition)$fill_empty(...)
# ds$where(condition)$copy_from(...)

# normalize_empty()
# reset_empty()
# recode_as_empty()
# refresh_empty()

# ds$sync_empty(P7D, 99)


# replace_with:
# ds$where(condition)$copy_from(Q1, source = Q2)


# ds$nvclone_to()
# clone_var()
# clone_vars()
# new_single()
# new_numeric()
# new_multiple()
# new_filter()

# ds$make_nps_groups(Q1, scale = 0:10)
# ds$make_nps_groups(Q1, scale = 1:11)
# make_nps_groups(vars, min = 0)
# make_nps_scores(vars, min_value = 0)
