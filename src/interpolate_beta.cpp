#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Rcpp::List cpp_interpolate_beta(
    Eigen::VectorXd beta,
    const Rcpp::IntegerVector& indices,
    const Rcpp::IntegerVector& neighbors,
    const Rcpp::IntegerVector& pointers,
    const double tol,
    const int maxit
) {

  const int n_missing = indices.size();
  double max_change = 0.0;
  int iter;
  bool converged = false;

  for (iter = 0; iter < maxit; ++iter) {
    max_change = 0.0;

    for (int i = 0; i < n_missing; ++i) {
      const int index = indices[i] - 1;
      const int start = pointers[i] - 1;
      const int end = pointers[i + 1] - 1; // start to end - 1

      double sum_neighbors = 0.0;
      for (int j = start; j < end; ++j) {
        const int neighbor = neighbors[j] - 1;
        sum_neighbors += beta(neighbor);
      }
      const double avg = sum_neighbors / (end - start);

      const double current_change = std::abs(beta(index) - avg);
      if (current_change > max_change) {
        max_change = current_change;
      }

      beta(index) = avg;
    }

    if (max_change < tol) {
      converged = true;
      break;
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("beta") = beta,
    Rcpp::Named("iter") = (converged) ? iter + 1 : maxit,
    Rcpp::Named("converged") = converged
  );
}
