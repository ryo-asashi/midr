#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Rcpp::List relax_beta_cpp(
    Eigen::VectorXd beta,
    const Rcpp::IntegerVector& m_indices,
    const Rcpp::IntegerVector& a_indices,
    const Rcpp::IntegerVector& pointers,
    const double tol,
    const int maxit) {

  const int n_missing = m_indices.size();
  double max_change = 0.0;
  int iter;

  for (iter = 0; iter < maxit; ++iter) {
    max_change = 0.0;

    for (int i = 0; i < n_missing; ++i) {
      const int m_index = m_indices[i] - 1;
      const int start = pointers[i] - 1;
      const int end = pointers[i + 1] - 1; // start to end - 1

      double sum_neighbors = 0.0;
      for (int j = start; j < end; ++j) {
        const int a_index = a_indices[j] - 1;
        sum_neighbors += beta(a_index);
      }
      const double avg = sum_neighbors / (end - start);

      const double current_change = std::abs(beta(m_index) - avg);
      if (current_change > max_change) {
        max_change = current_change;
      }
      
      beta(m_index) = avg;
    }

    if (max_change < tol) {
      break;
    }
  }
  
  return Rcpp::List::create(
    Rcpp::Named("beta") = beta,
    Rcpp::Named("iter") = (iter < maxit) ? iter + 1 : iter
  );
}