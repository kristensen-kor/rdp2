#' @include rdp2.R
#'
DS$set("private", "get_res_vec", function(total_sum, p) {
	if (total_sum == 0) {
		c(total_sum, rep(NA, length(row_values)))
	} else {
		c(total_sum, p / total_sum)
	}
})

DS$set("private", "calc_raw_table_single", function(vec, weights, row_values) {
	p = tapply(weights, vec, sum)[as.character(row_values)] |> unname()
	p[is.na(p)] = 0
	private$get_res_vec(sum(p), p)
})

DS$set("private", "calc_raw_table_multiple", function(vec, weights, row_values) {
	p = vapply(row_values, \(x) sum(weights[vapply(vec, \(y) any(y == x), logical(1))]), double(1))
	total_sum = sum(weights[lengths(vec) > 0])

	private$get_res_vec(total_sum, p)
})

DS$set("private", "calc_raw_table", function(var, weight = NULL) {
	vec = self$data[[var]]

	weights = if (is.null(weight)) rep(1, length(vec)) else self$data[[weight]]

	row_values = self$prepare_val_labels(var)

	if (is_multiple(vec)) {
		private$calc_raw_table_multiple(vec, weights, row_values)
	} else {
		private$calc_raw_table_single(vec, weights, row_values)
	}
})
