# class-ds.R

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


# Convenience wrapper around DS$new(...)
new_ds = function(...) DS$new(...)



# read/write

# Reads an SPSS (.sav) file and loads the data and metadata into the DS object.
DS$set("public", "get_spss", \(filename, encoding = NULL, haven = F) {
	start_time = Sys.time()

	if (!haven) {
		sav = read_sav(filename, encoding = encoding)

		self$data = as_tibble(sav$data, .name_repair = "minimal")
		self$var_labels = sav$var_labels
		self$val_labels = sav$val_labels
	} else {
		df_raw = haven::read_spss(filename)

		self$data = df_raw |> modify(\(x) `attributes<-`(x, NULL))
		attr(self$data, "label") = NULL
		self$var_labels = df_raw |> map(\(x) attr(x, "label", exact = T)) |> compact()
		self$val_labels = df_raw |> map(\(x) attr(x, "labels", exact = T)) |> compact()
	}

	message(glue("Read SPSS: {elapsed_fmt(Sys.time() - start_time)} ({self$nrow} rows, {length(self$variables)} variables)"))
	invisible(NULL)
})

# Loads data and metadata from an RDS file into the DS object.
DS$set("public", "get_rds", \(filename) {
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
DS$set("public", "initialize", \(filename = NULL, encoding = NULL, haven = F) {
	if (!is.null(filename)) {
		checkmate::assert_string(filename, min.chars = 1)

		if (tools::file_ext(filename) == "") filename = paste0(filename, ".rds")

		if (!file.exists(filename)) stop("File does not exist: ", filename, call. = F)

		file_extension = tolower(tools::file_ext(filename))

		if (file_extension == "sav") {
			self$get_spss(filename, encoding, haven)
		} else if (file_extension == "rds") {
			self$get_rds(filename)
		} else {
			stop("Unknown file format: ", file_extension, ". Only .rds and .sav formats are supported.", call. = F)
		}
	}

	invisible(NULL)
})

# Saves the current data and metadata of the DS object to an RDS file.
DS$set("public", "save", \(filename) {
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

	checkmate::assert_character(prefixes, min.len = 1, min.chars = 1, any.missing = F)

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
is_multiple = \(x) is.list(x)

# Determines and returns the type of specified variables in the dataset.
DS$set("public", "var_type", \(...) {
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
DS$set("public", "filter", \(..., .quiet = F) {
	if (length(rlang::enquos(...)) == 0) stop("At least one filtering condition must be supplied.", call. = F)

	n_before = self$nrow
	self$data = self$data |> filter(...)
	n_removed = n_before - self$nrow

	if (!.quiet) message(glue("Filter: removed {n_removed} row{if (n_removed == 1) '' else 's'}; {self$nrow} remain."))

	invisible(NULL)
})

# Removes rows matching the supplied condition.
DS$set("public", "drop_if", \(condition, .quiet = F) {
	condition = rlang::enquo(condition)

	n_before = self$nrow
	self$data = self$data |> filter(!({{ condition }}))
	n_removed = n_before - self$nrow

	if (!.quiet) message(glue("Drop: removed {n_removed} row{if (n_removed == 1) '' else 's'}; {self$nrow} remain."))

	invisible(NULL)
})

# Retains only the specified variables in the dataset and associated metadata.
DS$set("public", "keep", \(..., .quiet = F) {
	if (length(rlang::enquos(...)) == 0) stop("At least one variable selection must be supplied.", call. = F)

	n_before = length(self$variables)
	self$data = self$data |> select(...)
	self$vacuum()
	n_removed = n_before - length(self$variables)

	if (!.quiet) message(glue("Keep: removed {n_removed} variable{if (n_removed == 1) '' else 's'}; {length(self$variables)} remain."))

	invisible(NULL)
})

# Removes the specified variables from the dataset and associated metadata.
DS$set("public", "remove", \(..., .quiet = F) {
	if (length(rlang::enquos(...)) == 0) stop("At least one variable selection must be supplied.", call. = F)

	n_before = length(self$variables)
	self$data = self$data |> select(-c(...))
	self$vacuum()
	n_removed = n_before - length(self$variables)

	if (!.quiet) message(glue("Remove: removed {n_removed} variable{if (n_removed == 1) '' else 's'}; {length(self$variables)} remain."))

	invisible(NULL)
})

# Changes the order of specified variables in the dataset.
DS$set("public", "move", \(..., after = NULL, before = NULL) {
	if (!rlang::quo_is_null(rlang::enquo(after)) && !rlang::quo_is_null(rlang::enquo(before))) stop("Only one of `after` and `before` can be supplied.", call. = F)
	self$data = self$data |> relocate(..., .after = {{ after }}, .before = {{ before }})
	invisible(NULL)
})

# Convenience wrapper that clones the dataset and keeps matching rows.
# DS fields contain value-semantics R objects; shallow R6 cloning is intentional.
DS$set("public", "clone_if", \(...) {
	tds = self$clone()
	tds$filter(..., .quiet = T)
	invisible(tds)
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

# Convenience wrapper that returns number of rows satisfying the condition.
DS$set("public", "count_if", \(condition) self$where({{ condition }})$nrow)

# ds$left_join(weights, by = "RID")

# has_all(Q1, 1, 3, 5)
# has_only(Q1, 1:3)

