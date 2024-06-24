#' @include rdp2.R

DS$set("public", "qtable", function(var, weight = NULL) {
	var_name = self$names({{ var }})
	vec = self$data[[var_name]]
	# vec = self$data |> pull({{ var }})

	weights = if (rlang::quo_is_null(enquo(weight))) rep(1, length(vec)) else self$data |> pull({{ weight }})

	# var_name = rlang::as_string(ensym(var))

	var_label = self$get_var_label(var_name)
	caption = if (!is.na(var_label)) paste(var_name, var_label, sep = "|") else var_name

	weights = weights[!is.na(vec)]
	vec = vec[!is.na(vec)]

	if (self$is_nominal(var_name)) {
		row_values = self$prepare_val_labels(var_name)

		res = calc_raw_table_nominal(vec, weights, row_values)
		res[1] = round(res[1])
		res[-1] = round(100 * res[-1], 1)

		unweighted_counts = calc_raw_counts(vec, row_values)

		knitr::kable(
			data.frame(
				c("", row_values),
				c("Total", names(row_values)),
				c(res),
				c(unweighted_counts)
			),
			col.names = c("", ifelse(is_multiple(vec), "multiple", "single"), "%", "cnt"),
			row.names = F,
			caption = caption,
			align = c("r", "l", "r", "r"),
			format = "simple"
		)
	} else {
		total_sum = sum(weights)
		m = if (total_sum > 0) weighted.mean(vec, weights) else NA

		knitr::kable(
			data.frame(
				c("total", "mean"),
				c(round(total_sum), round(m, 1))
			),
			col.names = NULL,
			caption = caption,
			align = c("r", "r"),
			format = "simple"
		)
	}
})
