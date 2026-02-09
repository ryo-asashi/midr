#define EIGEN_DONT_VECTORIZE

#include <RcppEigen.h>
#include <limits>
#include <cmath>

// [[Rcpp::depends(RcppEigen)]]

//Helper Function: calculate_rank()
inline int calculate_rank(const Eigen::ArrayXd& vals, int cols) {
  double threshold = std::numeric_limits<double>::epsilon() * cols * vals.maxCoeff();
  return (vals > threshold).count();
}

// Helper Function: invert_vals()
inline Eigen::ArrayXd invert_vals(const Eigen::ArrayXd& vals, int cols) {
  double threshold = std::numeric_limits<double>::epsilon() * cols * vals.maxCoeff();
  Eigen::ArrayXd inv = vals;
  for (int i = 0; i < vals.size(); ++i) {
    inv[i] = (vals[i] > threshold) ? (1.0 / vals[i]) : 0.0;
  }
  return inv;
}

// [[Rcpp::export]]
Rcpp::List fastLmMatrix(
    Rcpp::NumericMatrix Xr,
    Rcpp::NumericMatrix yr,
    int type = 0
) {

  const Eigen::Map<Eigen::MatrixXd> X(Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(Xr));
  const Eigen::Map<Eigen::MatrixXd> y(Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(yr));

  // const int n = X.rows();
  const int p = X.cols();

  Eigen::MatrixXd coef;
  int rank = 0;

  switch (type) {

  // Case 0: ColPivHouseholderQR
  case 0: {
    Eigen::ColPivHouseholderQR<Eigen::MatrixXd> qr(X);
    rank = qr.rank();
    coef = qr.solve(y);
    break;
  }

  // Case 1: HouseholderQR
  case 1: {
    Eigen::HouseholderQR<Eigen::MatrixXd> qr(X);
    rank = p;
    coef = qr.solve(y);
    break;
  }

  // Case 2: LLT (Cholesky)
  case 2: {
    Eigen::MatrixXd Xt = X.adjoint();
    Eigen::MatrixXd XtX = Xt * X;
    Eigen::LLT<Eigen::MatrixXd> llt(XtX);
    rank = p;
    Eigen::MatrixXd Xty = Xt * y;
    coef = llt.solve(Xty);
    break;
  }

  // Case 3: LDLT (Robust Cholesky)
  case 3: {
    Eigen::MatrixXd Xt = X.adjoint();
    Eigen::MatrixXd XtX = Xt * X;
    Eigen::LDLT<Eigen::MatrixXd> ldlt(XtX);
    rank = calculate_rank(ldlt.vectorD().array().abs(), p);
    Eigen::MatrixXd Xty = Xt * y;
    coef = ldlt.solve(Xty);
    break;
  }

  // Case 4: JacobiSVD
  case 4: {
    Eigen::JacobiSVD<Eigen::MatrixXd> svd(X, Eigen::ComputeThinU | Eigen::ComputeThinV);
    Eigen::ArrayXd s = svd.singularValues();
    rank = calculate_rank(s, p);
    Eigen::ArrayXd s_inv = invert_vals(s, p);
    Eigen::MatrixXd V = svd.matrixV();
    Eigen::MatrixXd V_Sin = V * s_inv.matrix().asDiagonal();
    Eigen::MatrixXd U = svd.matrixU();
    Eigen::MatrixXd Ut = U.adjoint();
    Eigen::MatrixXd Uty = Ut * y;
    coef = V_Sin * Uty;
    break;
  }

  // Case 5: SymmEigen
  case 5: {
    Eigen::MatrixXd Xt = X.adjoint();
    Eigen::MatrixXd XtX = Xt * X;
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eig(XtX);
    Eigen::ArrayXd vals = eig.eigenvalues();
    rank = calculate_rank(vals, p);
    Eigen::ArrayXd val_inv = invert_vals(vals, p);
    Eigen::MatrixXd Evec = eig.eigenvectors();
    Eigen::MatrixXd Evec_Valin = Evec * val_inv.matrix().asDiagonal();
    Eigen::MatrixXd Xty = Xt * y;
    Eigen::MatrixXd EvecT = Evec.adjoint();
    Eigen::MatrixXd term2 = EvecT * Xty;
    coef = Evec_Valin * term2;
    break;
  }

  default: {
    Eigen::ColPivHouseholderQR<Eigen::MatrixXd> qr(X);
    rank = qr.rank();
    coef = qr.solve(y);
    break;
  }
  }

  Eigen::MatrixXd fitted = X * coef;
  Eigen::MatrixXd resid = y - fitted;

  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("coefficients")  = coef,
    Rcpp::Named("fitted.values") = fitted,
    Rcpp::Named("residuals")     = resid,
    Rcpp::Named("rank")          = rank
  );

  if (Xr.hasAttribute("dimnames") || yr.hasAttribute("dimnames")) {
    Rcpp::List dx = Xr.attr("dimnames");
    Rcpp::List dy = yr.attr("dimnames");
    Rcpp::CharacterVector cnx = (dx.size() > 1) ? dx[1] : Rcpp::CharacterVector();
    Rcpp::CharacterVector cny = (dy.size() > 1) ? dy[1] : Rcpp::CharacterVector();

    if (cnx.size() > 0 || cny.size() > 0) {
      Rcpp::List dimnames = Rcpp::List::create(cnx, cny);
      Rcpp::NumericMatrix out_coef = out["coefficients"];
      out_coef.attr("dimnames") = dimnames;
    }
  }

  return out;
}
