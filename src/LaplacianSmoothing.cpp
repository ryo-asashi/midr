#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Rcpp::List LaplacianSmoothing(
    Eigen::MatrixXd coefficients,
    const Rcpp::IntegerVector& indices,
    const Rcpp::IntegerVector& adjacencies,
    const Rcpp::IntegerVector& pointers,
    const double tol,
    const int max_iter
) {

  const int n_missing = indices.size();
  const int n_cols = coefficients.cols();

  int iter = 0;
  bool converged = false;
  double max_change = 0.0;

  Eigen::RowVectorXd average(n_cols);
  Eigen::RowVectorXd diff(n_cols);

  for (iter = 0; iter < max_iter; ++iter) {

    if (iter % 100 == 0) {
      Rcpp::checkUserInterrupt();
    }

    max_change = 0.0;

    for (int i = 0; i < n_missing; ++i) {
      const int index = indices[i] - 1;
      const int start = pointers[i] - 1;
      const int end   = pointers[i + 1] - 1;

      if (end <= start) continue;

      average.setZero();
      for (int j = start; j < end; ++j) {
        const int adjacency = adjacencies[j] - 1;
        average += coefficients.row(adjacency);
      }
      average /= (double)(end - start);

      diff = (coefficients.row(index) - average).cwiseAbs();
      double current_max_change = diff.maxCoeff();

      if (current_max_change > max_change) {
        max_change = current_max_change;
      }

      coefficients.row(index) = average;
    }

    if (max_change < tol) {
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
