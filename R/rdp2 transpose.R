#' @include rdp2.R

# The vars_transpose method transforms a set of multiple-response variables with numeric suffixes into a single consolidated multiple-response variable.
# It reorganizes the data by assigning new names and labels with a specified prefix, handles the exclusion of certain codes, and replaces the original variables with the newly transposed variable in the dataset.
DS$set("public", "vars_transpose", function(vars, new_name, label_prefix, na_code, remove_code) {
	vars = self$names({{ vars }})

	ids_from = strsplit(vars, "_") |> map_chr(\(x) x[length(x)])

	if (!(all(grepl("^[0-9]+$", ids_from)) && length(unique(as.numeric(ids_from))) == length(as.numeric(ids_from)))) stop("Error: Wrong input vars format")

	ids_from = as.numeric(ids_from)

	order_indices = order(ids_from)
	vars = vars[order_indices]
	ids_from = ids_from[order_indices]

	values_from = self$val_labels[[vars[1]]]
	values_from = values_from[!(values_from %in% remove_code)]

	labels = setNames(ids_from, self$get_var_labels(all_of(vars)))

	values_from |> iwalk(\(value, name) self$nvm(paste_vars(new_name, value), paste0(label_prefix, name), labels))

	walk2(self$base_name(new_name), values_from, \(var_name, value) {
		self$data[[var_name]] = do.call(rbind, map2(vars, ids_from, \(var_from, id) {
			if_else(has(self$data[[var_from]], value), id, NA_real_)
		})) |> as.data.frame() |> as.list() |> lapply(\(x) x[!is.na(x)]) |> unname()
	})

	self$recode_empty(base(new_name), na_code, "None of the above")

	self$move(base(new_name), after = all_of(vars))

	self$remove(all_of(vars))
})
