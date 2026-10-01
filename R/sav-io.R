# sav-io.R

read_sav = function(path, encoding = NULL, user_na = F, debug = F) {
	.Call("sav_read_c", path, encoding, user_na, debug, PACKAGE = "rdp2")
}

write_sav = function(x, path, encoding = "UTF-8") {
	invisible(.Call("sav_write_c", x, path, encoding, PACKAGE = "rdp2"))
}
