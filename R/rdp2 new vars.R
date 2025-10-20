#' @include rdp2.R

# Internal generic method for creating new variables with specified attributes.
DS$set("private", "nv_generic", function(name, label = NULL, labels = NULL, fill = NA_real_, after = NULL, before = NULL, var_type = "single") {
	if (name %in% self$variables) cat("Warning:", name, "is already present. Replacing.\n")

	if (var_type == "single") {
		self$data = self$data |> mutate("{ name }" := fill)
	} else {
		self$data[[name]] = rep(list(numeric(0)), self$nrow)
	}

	if (xor(!rlang::quo_is_null(enquo(after)), !rlang::quo_is_null(enquo(before)))) self$data = self$data |> relocate(all_of(name), .after = {{ after }}, .before = {{ before }})
	self$var_labels[[name]] = label
	if (!is.null(labels)) {
		self$set_val_labels({{ name }}, labels)
	} else {
		self$val_labels[[name]] = NULL
	}
})

# Creates a new single-response variable with specified attributes.
DS$set("public", "nvn", function(name, label = NULL, labels = NULL, fill = NA_real_, after = NULL, before = NULL) {
	private$nv_generic(name, label, labels, fill, {{ after }}, {{ before }}, "single")
})

# Creates a new numeric variable without value labels.
DS$set("public", "nvs", function(name, label = NULL, fill = NA_real_, after = NULL, before = NULL) {
	private$nv_generic(name, label, labels = NULL, fill, {{ after }}, {{ before }}, "single")
})

# Creates a new multiple-response variable with specified attributes.
DS$set("public", "nvm", function(name, label = NULL, labels = NULL, after = NULL, before = NULL) {
	private$nv_generic(name, label, labels, NULL, {{ after }}, {{ before }}, "multiple")
})

# Creates a new single-response filter subtype variable.
DS$set("public", "nvf", function(name, label = NULL, labels = NULL, after = NULL, before = NULL, filter) {
	if (missing(filter)) stop("Please specify filter condition", call. = F)
	self$nvn(name, label, after = {{ after }}, before = {{ before }})
	self$set_if({{ name }}, 1, label = labels, {{ filter }})
})

# Adds a total single-select variable with a fixed value of 1.
DS$set("public", "add_total", function(after = NULL, before = NULL) {
	self$nvn("total", "Total", c("Total" = 1), fill = 1, after = after, before = before)
})

# Adds a respondent ID (RID) variable with sequential numbering.
DS$set("public", "add_rid", function(name = "RID", label = "Respondent ID", after = NULL, before = NULL) {
	self$nvs(name, label, fill = row_number(), after = after, before = before)
})


