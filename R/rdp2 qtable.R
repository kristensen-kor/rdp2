#' @include rdp2.R

# add % total

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


# DS$set("public", "qctable", function(var, by, weight = NULL) {
#
# })


# qctable = function(self, var, col, weight = NULL) {
# 	var_name = self$names({{ var }})
# 	col_name = self$names({{ col }})
# 	vec = self$data[[var_name]]
# 	col_vec = self$data[[col_name]]
# 	# vec = self$data |> pull({{ var }})
#
# 	weights = if (rlang::quo_is_null(enquo(weight))) rep(1, length(vec)) else self$data |> pull({{ weight }})
#
# 	# var_name = rlang::as_string(ensym(var))
#
# 	var_label = self$get_var_label(var_name)
# 	col_label = self$get_var_label(col_name)
#
# 	caption = if (!is.na(var_label)) paste(var_name, var_label, sep = "|") else var_name
# 	caption_col = if (!is.na(col_label)) paste(col_name, col_label, sep = "|") else col_name
#
# 	# weights = weights[!is.na(vec)]
# 	# vec = vec[!is.na(vec)]
#
# 	knitr::kable(
# 		data.frame(
# 			c("", "row_values"),
# 			c("Total", "names(row_values)"),
# 			c(1:2),
# 			c(1:2)
# 		),
# 		col.names = c("", ifelse(is_multiple(vec), "multiple", "single"), "%", "cnt"),
# 		row.names = F,
# 		caption = paste0(caption, "\n   by: ", caption_col),
# 		align = c("r", "l", "r", "r"),
# 		format = "simple"
# 	)
# }
#
# qctable(ds, REGION, TYPE)
#
# ds$variables
#
# ?set_names
# 1:5 |> set_names()
# c("a", "b") |> set_names()
#
# ds$var_view(check = T) |> View()
#
# ds$qtable(Gender)
# ds$qtable(Age)
# ds$qtable(REGION)
# ds$qtable(TYPE)
# ds$qtable(Z3a2)
# ds$qtable(Z3a3)
