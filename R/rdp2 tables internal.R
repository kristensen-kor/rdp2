#' @include rdp2.R
#'

calc_raw_table_nominal = function(vec, weights, row_values) {
	total_sum = sum(weights[is_valid(vec)])

	if (total_sum == 0) {
		c(total_sum, rep(NA, length(row_values)))
	} else {
		c(total_sum, vapply(row_values, \(x) sum(weights[has(vec, x)]), double(1)) / total_sum)
	}
}

DS$set("private", "calc_raw_table", function(var, weight = NULL) {
	vec = self$data[[var]]

	weights = if (is.null(weight)) rep(1, length(vec)) else self$data[[weight]]

	row_values = self$prepare_val_labels(var)

	calc_raw_table_nominal(vec, weights, row_values)
})

calc_raw_table_mean = function(vec, weights) {
	total_sum = sum(weights)

	if (total_sum == 0) {
		m = NA
		s = NA
	} else {
		m = sum(weights * vec) / sum(weights) # weighted.mean(vec, weights)
		s = (sum(weights * (vec - m) ^ 2) / (sum(weights) - 1)) ^ 0.5
	}

	c(total_sum, m, s)
}


# calc_raw_counts = function(vec, row_values) {
# 	c(sum(is_valid(vec)), vapply(row_values, \(x) sum(has(vec, x)), integer(1)))
# }

calc_raw_counts = function(...) UseMethod("calc_raw_counts")

calc_raw_counts.list = function(vec, row_values) {
	c(sum(lengths(vec) > 0), vapply(row_values, \(x) sum(has(vec, x)), integer(1)))
}

calc_raw_counts.default = function(vec, row_values) {
	cnts = table(vec)[as.character(row_values)] |> unname()
	cnts[is.na(cnts)] = 0
	c(sum(cnts), cnts)
}
