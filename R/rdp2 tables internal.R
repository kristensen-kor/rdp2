#' @include rdp2.R
#'

get_res_vec = function(total_sum, p) {
	if (total_sum == 0) {
		c(total_sum, rep(NA, length(p)))
	} else {
		c(total_sum, p / total_sum)
	}
}

calc_raw_table_nominal = function(...) UseMethod("calc_raw_table_nominal")

calc_raw_table_nominal.list = function(vec, weights, row_values) {
	p = vapply(row_values, \(x) sum(weights[has(vec, x)]), double(1))
	total_sum = sum(weights[lengths(vec) > 0])

	get_res_vec(total_sum, p)
}

calc_raw_table_nominal.default = function(vec, weights, row_values) {
	p = tapply(weights, vec, sum)[as.character(row_values)] |> unname()
	p[is.na(p)] = 0

	get_res_vec(sum(p), p)
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


calc_raw_counts = function(...) UseMethod("calc_raw_counts")

calc_raw_counts.list = function(vec, row_values) {
	list(sum_cnts = sum(lengths(vec) > 0), cnts = vapply(row_values, \(x) sum(has(vec, x)), integer(1)))
}

calc_raw_counts.default = function(vec, row_values) {
	cnts = table(vec)[as.character(row_values)] |> unname()
	cnts[is.na(cnts)] = 0
	list(sum_cnts = sum(cnts), cnts = cnts)
}
