# sav-io.R

read_sav = function(path, encoding = NULL, user_na = F, debug = F) {
	.Call("sav_read_c", path, encoding, user_na, debug, PACKAGE = "rdp2")
}

write_sav = function(x, path, encoding = "UTF-8") {
	if ((!is.list(x) && !is.environment(x)) || !all(c("data", "var_labels", "val_labels") %in% names(x))) {
		stop("Expected an object with data, var_labels and val_labels fields.", call. = F)
	}

	invisible(.Call("sav_write_c", list(data = x$data, var_labels = x$var_labels, val_labels = x$val_labels), path, encoding, PACKAGE = "rdp2"))
}
