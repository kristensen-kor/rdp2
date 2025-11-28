#include <Rcpp.h>

using namespace Rcpp;

NumericVector add_value(NumericVector vec, double value) {
	if (R_IsNA(value)) return vec;

	size_t n = vec.size();
	if (n == 0) return NumericVector::create(value);

	auto begin = vec.begin();
	auto end = vec.end();
	auto it = std::lower_bound(begin, end, value);

	if (it != end && *it == value) return vec;

	NumericVector out(n + 1);
	std::copy(begin, it, out.begin());
	out[it - begin] = value;
	std::copy(it, end, out.begin() + (it - begin + 1));

	return out;
}

// [[Rcpp::export]]
List add_to_mc_col_cpp(List data, double value) {
	const size_t n = data.size();
	List out(n);

	for (size_t i = 0; i < n; i++) {
		SEXP element = data[i];

		if (!Rf_isReal(element)) stop("All elements of `data` must be numeric vectors.");

		NumericVector vec(element);
		out[i] = add_value(vec, value);
	}

	return out;
}
