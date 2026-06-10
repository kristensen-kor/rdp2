#' DS Class
#'
#' The `DS` class manages both raw data and its associated metadata, facilitating streamlined data manipulation.
#'
#' @field data A tibble containing the dataset, with each column representing a different variable (e.g., age, gender, survey responses).
#' @field var_labels A named list mapping each variable's identifier to a descriptive label, enhancing readability in outputs and reports.
#' @field val_labels A named list for categorical variables, where each entry maps numeric codes to meaningful category labels (e.g., 1 = "Agree", 2 = "Disagree").
#'
#' @export
#' @noRd
DS = R6::R6Class("DS", list(
	data = tibble(),
	var_labels = list(),
	val_labels = list()
))



# read/write

# Reads an SPSS (.sav) file and loads the data and metadata into the DS object.
DS$set("public", "get_spss", function(filename) {
	start_time = Sys.time()

	df_raw = haven::read_spss(filename)

	self$data = df_raw |> modify(\(x) `attributes<-`(x, NULL))
	self$var_labels = df_raw |> map(\(x) attr(x, "label", exact = T)) |> compact()
	self$val_labels = df_raw |> map(\(x) attr(x, "labels", exact = T)) |> compact()

	message(glue("Read SPSS: {elapsed_fmt(Sys.time() - start_time)} ({self$nrow} rows, {length(self$variables)} variables)"))
	invisible(NULL)
})

# Loads data and metadata from an RDS file into the DS object.
DS$set("public", "get_rds", function(filename) {
	save_data = readRDS(filename)

	if (!inherits(save_data$data, "data.frame")) stop("Invalid rdp2 file: `data` must be a data frame.", call. = F)
	if (!is.list(save_data$var_labels)) stop("Invalid rdp2 file: `var_labels` must be a list.", call. = F)
	if (!is.list(save_data$val_labels)) stop("Invalid rdp2 file: `val_labels` must be a list.", call. = F)

	self$data = save_data$data
	self$var_labels = save_data$var_labels
	self$val_labels = save_data$val_labels

	invisible(NULL)
})

# Initializes an empty dataset or loads an RDS/SPSS file; extensionless paths default to .rds.
DS$set("public", "initialize", function(filename = NULL) {
	if (!is.null(filename)) {
		assert_nonempty_string(filename)

		if (tools::file_ext(filename) == "") filename = paste0(filename, ".rds")

		if (!file.exists(filename)) stop("File does not exist: ", filename, call. = F)

		file_extension = tools::file_ext(filename) |> tolower()

		if (file_extension == "sav") {
			self$get_spss(filename)
		} else if (file_extension == "rds") {
			self$get_rds(filename)
		} else {
			stop("Unknown file format: ", file_extension, ". Only .rds and .sav formats are supported.", call. = F)
		}
	}

	invisible(NULL)
})

# Saves the current data and metadata of the DS object to an RDS file.
DS$set("public", "save", function(filename) {
	assert_nonempty_string(filename)

	if (tools::file_ext(filename) == "") filename = paste0(filename, ".rds")

	save_data = list(format = "rdp2", version = 1L, var_labels = self$var_labels, val_labels = self$val_labels, data = self$data)
	saveRDS(save_data, file = filename)
})


# basic

# Active binding that returns the names of variables in the dataset.
DS$set("active", "variables", \() names(self$data))

# Active binding that returns the number of rows in the dataset.
DS$set("active", "nrow", \() nrow(self$data))


# selection

#' Selects variables named PREFIX_1, PREFIX_2, ..., for one or more literal prefixes.
#' @export
base = function(...) {
	prefixes = c(...)

	if (length(prefixes) == 0) stop("At least one variable prefix must be supplied.", call. = F)
	if (!is.character(prefixes) || anyNA(prefixes) || any(prefixes == "")) stop("Variable prefixes must be non-empty, non-missing character values.", call. = F)

	# Escapes regular-expression metacharacters in literal strings.
	prefixes_esc = gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", prefixes)

	matches(paste0("^", prefixes_esc, "_\\d+$"))
}

# Returns column names selected via tidyselect syntax.
DS$set("public", "names", \(...) {
	if (length(rlang::enquos(...)) == 0) stop("No variable selection supplied. Use `ds$variables` to get all variable names.", call. = F)

	self$data |> select(...) |> names()
})

# Convenience wrapper around $names(base(...))
DS$set("public", "base_name", \(...) self$names(base(...)))



# types

#' Checks whether a vector uses the rdp2 multiple-response representation.
#' @export
is_multiple = \(x) is.list(x) && !is.data.frame(x) && all(vapply(x, is.numeric, logical(1)))

# Determines and returns the type of specified variables in the dataset.
DS$set("public", "var_type", function(...) {
	self$names(...) |> map_chr(\(var_name) {
		var = self$data[[var_name]]

		if (is.numeric(var) && var_name %in% names(self$val_labels)) {
			"single"
		} else if (is_multiple(var)) {
			"multiple"
		} else if (is.numeric(var)) {
			"numeric"
		} else if (is.character(var)) {
			"text"
		} else {
			warning("Variable does not match any expected type: ", var_name, call. = F)
			NA_character_
		}
	})
})

# Checks if the specified variables are nominal (single or multiple categorical).
DS$set("public", "is_nominal", \(...) self$var_type(...) %in% c("single", "multiple"))




# Keeps rows matching the supplied conditions.
DS$set("public", "filter", function(...) {
	if (length(rlang::enquos(...)) == 0) stop("At least one filtering condition must be supplied.", call. = F)

	n_before = self$nrow

	self$data = self$data |> filter(...)

	# if (.verbose) {
		n_removed = n_before - self$nrow
		message(glue("Filter: removed {n_removed} row{if (n_removed == 1) '' else 's'}; {self$nrow} remain."))
		# plural_s = \(n) if (n == 1) "" else "s"
		# message(glue("Filtered out {n_removed} row{plural_s(n_removed)}; {self$nrow} remain."))
	# }

	invisible(NULL)
})

# Removes metadata entries for variables no longer present in the dataset.
DS$set("public", "vacuum", function() {
	self$var_labels = self$var_labels[names(self$var_labels) %in% self$variables]
	self$val_labels = self$val_labels[names(self$val_labels) %in% self$variables]
	invisible(NULL)
})

# Retains only the specified variables in the dataset and associated metadata.
DS$set("public", "keep", function(...) {
	if (length(rlang::enquos(...)) == 0) stop("At least one variable selection must be supplied.", call. = F)

	self$data = self$data |> select(...)
	self$vacuum()

	invisible(NULL)
})

# Removes the specified variables from the dataset and associated metadata.
DS$set("public", "remove", function(...) {
	if (length(rlang::enquos(...)) == 0) stop("At least one variable selection must be supplied.", call. = F)

	self$data = self$data |> select(-c(...))
	self$vacuum()

	invisible(NULL)
})

# Changes the order of specified variables in the dataset.
DS$set("public", "move", function(..., after = NULL, before = NULL) {
	self$data = self$data |> relocate(..., .after = {{ after }}, .before = {{ before }})
	invisible(NULL)
})

# Convenience wrapper that clones the dataset and keeps matching rows.
# DS fields contain value-semantics R objects; shallow R6 cloning is intentional.
DS$set("public", "clone_if", function(...) {
	tds = self$clone()
	tds$filter(...) |> suppressMessages()
	invisible(tds)
})





# Converts SPSS-style binary indicator groups into native multiple-response variables.
DS$set("public", "conv_multiples", function(sep = ": ", labels = c("-" = 0, "+" = 1)) {
	start_time = Sys.time()

	assert_nonempty_string(sep)
	if (!is.numeric(labels) || is.null(names(labels)) || length(labels) == 0 || anyNA(labels) || any(names(labels) == "")) {
		stop("`labels` must be a named numeric vector without missing values.", call. = F)
	}

	mdset_data = tibble(var_name = self$variables) |>
		filter(grepl("_[0-9]+$", var_name)) |>
		filter(map_lgl(var_name, \(x) identical(self$val_labels[[x]], labels))) |>
		mutate(label = self$get_var_labels(all_of(var_name))) |>
		filter(!is.na(label)) |>
		mutate(tokens = strsplit(label, sep, fixed = T)) |>
		mutate(base_name = sub("_[0-9]+$", "", var_name)) |>
		group_by(base_name) |>
		filter(all(lengths(tokens) > 1)) |>
		ungroup() |>
		mutate(prefix = map_chr(tokens, \(x) x[1])) |>
		mutate(label = map_chr(tokens, \(x) paste(x[-1], collapse = sep))) |>
		select(-tokens) |>
		mutate(id = sub(".*_([0-9]+)$", "\\1", var_name) |> as.numeric()) |>
		group_by(base_name) |>
		filter(n_distinct(prefix) == 1, anyDuplicated(id) == 0) |>
		ungroup() |>
		arrange(base_name, id)

	mdsets = mdset_data$base_name |> unique()

	if (length(mdsets) == 0) {
		message("No multiple-response sets found: ", elapsed_fmt(Sys.time() - start_time))
		return(invisible(NULL))
	}

	existing_targets = intersect(mdsets, self$variables)
	if (length(existing_targets) > 0) {
		stop("Cannot convert multiple-response sets because target variables already exist: ", paste(existing_targets, collapse = ", "), ".", call. = F)
	}

	n_indicators = nrow(mdset_data)

	for (mdset in mdsets) {
		current_slice = mdset_data[mdset_data$base_name == mdset, ]

		col_data = do.call(rbind, seq_len(nrow(current_slice)) |> lapply(\(i) {
			if_else(has(self$data[[current_slice$var_name[i]]], 1), current_slice$id[i], NA_real_)
		})) |> as.data.frame() |> as.list() |> lapply(\(x) x[!is.na(x)]) |> unname()

		self$data[[mdset]] = col_data
		self$data = self$data |> relocate(all_of(mdset), .before = all_of(current_slice$var_name[1]))
		self$data[current_slice$var_name] = NULL

		self$var_labels[[mdset]] = current_slice$prefix[1]
		self$val_labels[[mdset]] = setNames(current_slice$id, current_slice$label)
	}

	# to test new alternative optimized approach.
	# mdset_slices = split(mdset_data, mdset_data$base_name)
	#
	# new_columns = setNames(vector("list", length(mdsets)), mdsets)
	# anchors = setNames(character(length(mdsets)), mdsets)
	#
	# for (mdset in mdsets) {
	# 	current_slice = mdset_slices[[mdset]]
	#
	# 	new_columns[[mdset]] = do.call(rbind, seq_len(nrow(current_slice)) |> lapply(\(i) {
	# 		if_else(has(self$data[[current_slice$var_name[i]]], 1), current_slice$id[i], NA_real_)
	# 	})) |> as.data.frame() |> as.list() |> lapply(\(x) x[!is.na(x)]) |> unname()
	#
	# 	anchors[[mdset]] = current_slice$var_name[1]
	#
	# 	self$var_labels[[mdset]] = current_slice$prefix[1]
	# 	self$val_labels[[mdset]] = setNames(current_slice$id, current_slice$label)
	# }
	#
	# multiple_data = as_tibble(new_columns)
	#
	# final_names = names(self$data)
	# final_names[match(unname(anchors), final_names)] = names(anchors)
	# final_names = final_names[!final_names %in% mdset_data$var_name]
	#
	# self$data = bind_cols(self$data, multiple_data) |> select(all_of(final_names))


	self$vacuum()

	message(glue("Converted {length(mdsets)} multiple-response set{if (length(mdsets) == 1) '' else 's'} from {n_indicators} indicator variable{if (n_indicators == 1) '' else 's'}: {elapsed_fmt(Sys.time() - start_time)}"))

	invisible(NULL)
})



# Creates a temporary row-scoped context for the supplied condition.
DS$set("public", "where", function(condition) {
	invisible(DSWhere$new(self, rlang::enquo(condition)))
	# invisible(DSWhere$new(self, {{ condition }}))
})

# Creates a temporary row context for rows containing any supplied values.
DS$set("public", "where_has", function(var, ...) {
	self$where(has({{ var }}, ...))
})

# consider
DS$set("public", "where_not_has", function(var, ...) {
	self$where(!has({{ var }}, ...))
})

# where_empty()
# where_valid()
# where_between()
# where_missing()

# Convenience wrapper that returns cimber of rows satisfying the condition.
DS$set("public", "count_if", \(condition) self$where({{ condition }})$nrow)

# ds$left_join(weights, by = "RID")

# has_all(Q1, 1, 3, 5)
# has_only(Q1, 1:3)

