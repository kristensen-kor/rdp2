#include <Rcpp.h>
#include <unordered_set>

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector has_list_multiple_cpp(List list, NumericVector values) {
	int n = list.size();
	LogicalVector result(n, false);

	int n_val = values.size();
	double* p_vals = REAL(values);

	for (int i = 0; i < n; i++) {
		SEXP elem = list[i];
		int vec_len = Rf_length(elem);
		if (vec_len == 0) continue;
		bool found = false;

		if (TYPEOF(elem) == INTSXP) {
			int* p_elem = INTEGER(elem);
			for (int j = 0; j < vec_len && !found; j++) {
				int cur_int = p_elem[j];
				double* cur_val = p_vals;
				for (int k = 0; k < n_val; k++, cur_val++) {
					if (static_cast<double>(cur_int) == *cur_val) {
						found = true;
						break;
					}
				}
			}
		} else if (TYPEOF(elem) == REALSXP) {
			double* p_elem = REAL(elem);
			for (int j = 0; j < vec_len && !found; j++) {
				double cur_val_elem = p_elem[j];
				double* cur_val = p_vals;
				for (int k = 0; k < n_val; k++, cur_val++) {
					if (cur_val_elem == *cur_val) {
						found = true;
						break;
					}
				}
			}
		} else {
			stop("Element %d of the list is not numeric (integer or double).", i + 1);
		}

		result[i] = found;
	}
	return result;
}

// [[Rcpp::export]]
LogicalVector has_list_single_cpp(List xs, double y) {
	int n = xs.size();
	LogicalVector result(n, false);

	for (int i = 0; i < n; i++) {
		SEXP elem = xs[i];
		int vec_len = Rf_length(elem);
		if (vec_len == 0) continue;

		if (TYPEOF(elem) == INTSXP) {
			int* p = INTEGER(elem);
			for (int j = 0; j < vec_len; j++) {
				if (p[j] == y) {
					result[i] = true;
					break;
				}
			}
		} else if (TYPEOF(elem) == REALSXP) {
			double* p = REAL(elem);
			for (int j = 0; j < vec_len; j++) {
				if (p[j] == y) {
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
