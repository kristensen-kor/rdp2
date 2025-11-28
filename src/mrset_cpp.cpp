#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mrcheck_cpp(NumericVector xs) {
	const size_t n = xs.size();

	if (n == 1 && !R_IsNA(xs[0])) return xs;

	NumericVector out(n);
	size_t m = 0;

	for (size_t i = 0; i < n; i++) {
		double v = xs[i];
		if (!R_IsNA(v)) out[m++] = v;
	}

	if (m == 0) return NumericVector(0);

	std::sort(out.begin(), out.begin() + m);

	size_t u = 1;
	for (size_t i = 1; i < m; i++) {
		if (out[i] != out[i - 1]) out[u++] = out[i];
	}

	out.erase(out.begin() + u, out.end());

	return out;
}

// [[Rcpp::export]]
NumericVector add_to_mrset_cpp(NumericVector vec, double value) {
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
