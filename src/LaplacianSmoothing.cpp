#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

// [[Rcpp::export]]
Rcpp::List LaplacianSmoothing(
    Rcpp::NumericMatrix coefficients,
    const Rcpp::IntegerVector& targets,
    const Rcpp::IntegerVector& adjacencies,
    const Rcpp::IntegerVector& pointers,
    const double tol = 0.0000001,
    const int max_iter = 10000
) {

  const int n_missing = targets.size();
  const int n_cols = coefficients.ncol();

  int iter = 0;
  bool converged = false;
  double max_diff = 0.0;

  std::vector<double> average(n_cols);

  for (iter = 0; iter < max_iter; ++iter) {

    if (iter % 100 == 0) {
      Rcpp::checkUserInterrupt();
    }

    max_diff = 0.0;

    for (int i = 0; i < n_missing; ++i) {
      const int target = targets[i] - 1;
      const int start  = pointers[i] - 1;
      const int end    = pointers[i + 1] - 1;

      if (end <= start) continue;

      std::fill(average.begin(), average.end(), 0.0);

      for (int j = start; j < end; ++j) {
        const int adjacency = adjacencies[j] - 1;
        for (int k = 0; k < n_cols; ++k) {
          average[k] += coefficients(adjacency, k);
        }
      }

      double degree_inv = 1.0 / (double)(end - start);
      for (int k = 0; k < n_cols; ++k) {
        double new_val = average[k] * degree_inv;
        double d = std::abs(coefficients(target, k) - new_val);
        if (d > max_diff) {
          max_diff = d;
        }
        coefficients(target, k) = new_val;
      }
    }

    if (max_diff < tol) {
      converged = true;
      iter++;
      break;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("coefficients") = coefficients,
    Rcpp::Named("converged")    = converged,
    Rcpp::Named("n_iter")       = iter
  );
}
