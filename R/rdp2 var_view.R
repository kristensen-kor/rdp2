#' @include rdp2.R

# Provides a summary view of variables, including their names, labels, types, and value labels, optionally filtered by name or label.
DS$set("public", "var_view", function(name = NULL, label = NULL) {
	val_labels_format = function(xs) {
		if (is.null(xs)) return(NA_character_)
		paste(glue("[{xs}]"), chartr("\t\n", "  ", names(xs)), collapse = "; ")
	}

	res = tibble(
		pos = seq_along(self$variables),
		variable = self$variables,
		label = self$get_var_labels(everything())
	)

	if (!is.null(name)) {
		if (is.numeric(name)) {
			res = res |> slice(name)
		} else {
			res = res |> filter(grepl(name, variable))
		}
	}

	if (!is.null(label)) {
		res = res |> filter(grepl(.env$label, .data$label))
	}

	res = res |> mutate(
		type = self$var_type(all_of(variable)),
		type = ifelse(is.na(type), "type error", type),
		type = ifelse(type == "single", NA_character_, type),
		val_labels = variable |> map_chr(\(var_name) val_labels_format(self$val_labels[[var_name]]))
	) |> select(pos, variable, type, label, everything())

	res
})


# Checks variables for properties like uniqueness, emptiness, and validity based on specified criteria.
DS$set("public", "var_check", function(name = NULL, label = NULL) {
	res = self$var_view(name, label)

	res = res |> mutate(
		unique_values = variable |> map(\(var) self$data[[var]] |> unlist() |> unique() |> discard(\(x) is.na(x) || x == "")),
		empty = ifelse(lengths(unique_values) == 0, "1", ""),
		same = ifelse(lengths(unique_values) == 1, "1", ""),
		valid = variable |> map_dbl(\(var) self$data[[var]] |> map(\(x) if (all(is.na(x)) || all(x == "")) NULL else x) |> compact() |> length()),
		distinct = lengths(unique_values),
		unique_values = NULL
	)

	res
})
