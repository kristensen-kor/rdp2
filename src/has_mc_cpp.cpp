#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
LogicalVector has_mc_cpp(List list, NumericVector values) {
	const size_t list_length = list.size();
	const size_t values_length = values.size();

	LogicalVector result(list_length, false);

	// single value path
	if (values_length == 1) {
		const double target = values[0];

		for (size_t i = 0; i < list_length; i++) {
			SEXP elem = list[i];
			const size_t vec_len = Rf_length(elem);
			if (vec_len == 0) continue;

			if (TYPEOF(elem) == INTSXP) {
				const int* p_elem = INTEGER(elem);
				for (size_t j = 0; j < vec_len; j++) {
					if (p_elem[j] == target) {
						result[i] = true;
						break;
					}
				}
			} else if (TYPEOF(elem) == REALSXP) {
				const double* p_elem = REAL(elem);
				for (size_t j = 0; j < vec_len; j++) {
					if (p_elem[j] == target) {
						result[i] = true;
						break;
					}
				}
			} else {
				stop("Element %d of the list is not numeric (integer or double).", i + 1);
			}
		}

		return result;
	}

	uint64_t mask = 0;
	vector<double> fallback;
	fallback.reserve(values_length);

	for (auto v : values) {
		if (v >= 0 && v < 64 && v == (int)v) {
			mask |= (1ULL << (int)v);
		} else {
			fallback.push_back(v);
		}
	}

	const size_t fsize = fallback.size();

	for (size_t i = 0; i < list_length; i++) {
		SEXP elem = list[i];
		const size_t vec_len = Rf_length(elem);
		if (vec_len == 0) continue;

		if (TYPEOF(elem) == INTSXP) {
			const int* p_elem = INTEGER(elem);

			for (size_t j = 0; j < vec_len; j++) {
				const int v = p_elem[j];
				if (v >= 0 && v < 64 && (mask & (1ULL << v))) {
					result[i] = true;
					break;
				} else {
					for (size_t k = 0; k < fsize; k++) {
						if (v == fallback[k]) {
							result[i] = true;
							goto next;
						}
					}
				}
			}
		} else if (TYPEOF(elem) == REALSXP) {
			const double* p_elem = REAL(elem);

			for (size_t j = 0; j < vec_len; j++) {
				const double vd = p_elem[j];

				if (vd == (int)vd) {
					int v = (int)vd;
					if (v >= 0 && v < 64 && (mask & (1ULL << v))) {
						result[i] = true;
						break;
					}
				}

				for (size_t k = 0; k < fsize; k++) {
					if (vd == fallback[k]) {
						result[i] = true;
						goto next;
					}
				}
			}
		} else {
			stop("Element %d of the list is not numeric (integer or double).", i + 1);
		}

		next: ;
	}

	return result;
}
