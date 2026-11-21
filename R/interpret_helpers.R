# R/interpret_helpers.R

# internal sub-processes of interpret()

preprocess_data <- function(
    object, x, y, weights, pred.fun, pred.args, link, na.action, verbosity
) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    x <- try(as.data.frame(x), silent = TRUE)
    if (inherits(x, "try-error")) {
      stop("'x' must be a matrix or data.frame, or an object that can be converted to a data.frame")
    }
  }
  colnames(x) <- clean_names(x, prefix = "X")
  naai <- list(n.init = nrow(x), ids = seq_len(nrow(x)))
  attr(x, "na.action") <- NULL
  x <- do.call(na.action, list(x))
  if (!is.null(naa.x <- stats::na.action(x))) {
    verbose(paste(length(naa.x), "observations with NAs in 'x' are omitted"),
            verbosity, 3L)
    if (!is.null(y)) {
      y <- if (is.matrix(y) || is.data.frame(y)) y[-naa.x, , drop = FALSE] else y[-naa.x]
    }
    weights <- weights[-naa.x]
    naai$ids <- naai$ids[-naa.x]
    attr(x, "na.action") <- NULL
  }
  if (is.null(y)) {
    y <- try(do.call(pred.fun, c(list(object, x), pred.args)), silent = TRUE)
    if (inherits(y, "try-error")) {
      stop("'y' is not supplied and the model prediction failed")
    }
    if (is.matrix(y) || is.data.frame(y)) {
      verbose(paste0(nrow(y), " x ", ncol(y), " predictions obtained from 'object': ",
                     examples(y, 3L)), verbosity, 3L)
    } else {
      verbose(paste0(length(y), " predictions obtained from 'object': ",
                     examples(y, 3L)), verbosity, 3L)
    }
  }
  if (is.factor(y) || is.character(y)) {
    ref <- if (is.factor(y)) levels(y)[1L] else sort(unique(as.vector(y)))[1L]
    y <- (y != ref)
  }
  y <- as.matrix(y)
  if (nrow(y) != nrow(x)) {
    stop("length of 'y' doesn't match the number of rows in 'x'")
  }
  ntargets <- ncol(y)
  if (ntargets > 1L) colnames(y) <- clean_names(y, prefix = "Y")
  rownames(y) <- NULL
  yres <- NULL
  if (!is.null(link)) {
    if (is.character(link)) link <- make_link(link)
    yres <- y
    y <- link$linkfun(y)
    verbose(paste0("'y' values are transformed by 'link': ", examples(y, 3L)),
            verbosity, 3L)
  }
  attr(y, "na.action") <- NULL
  y <- do.call(na.action, list(y))
  if (anyNA(y)) {
    stop("NA values in 'y' are not allowed")
  }
  if (!is.null(naa.y <- stats::na.action(y))) {
    verbose(paste(length(naa.y), "NA values in 'y' are omitted"), verbosity, 3L)
    x <- x[-naa.y, , drop = FALSE]
    weights <- weights[-naa.y]
    if (!is.null(link)) {
      yres <- yres[-naa.y, , drop = FALSE]
    }
    naai$ids <- naai$ids[-naa.y]
    attr(y, "na.action") <- NULL
  }
  if (any(is.infinite(y))) {
    stop("'Inf' and '-Inf' in 'y' are not allowed")
  }
  if (is.null(weights)) {
    weights <- rep.int(1, nrow(x))
  }
  if (!is.numeric(weights) || anyNA(weights)) {
    stop("'weights' must be a numeric vector without missing values")
  }
  if (length(weights) != nrow(x)) {
    stop("length of 'weights' doesn't match the number of rows in 'x'")
  }
  if (any(weights < 0)) {
    stop("negative weights not allowed")
  }
  if ((sumw <- sum(weights)) <= 0) {
    stop("sum of 'weights' must be strictly positive")
  }
  if (is.matrix(x)) x <- as.data.frame(x)
  n <- nrow(x)
  if (n == 0L) stop("no available observations found")
  weights <- weights / sumw * n
  list(
    x = x, y = y, yres = yres, weights = weights, link = link,
    n = n, ntargets = ntargets, naai = naai
  )
}


prepare_metadata <- function(
    x, weights, terms, interactions, k, type, encoders,
    split, digits, lump, others, sep, lambda, mode, fit.intercept,
    save.memory, weighted.encoding, nil, max.nelements, verbosity
) {
  n <- nrow(x)
  tags <- colnames(x)
  orvs <- sapply(x, is.numeric) | sapply(x, is.ordered)
  terms <- as_terms(
    terms %||% if (interactions) ".^2" else ".",
    intercept = TRUE, data = x
  )
  labs <- get_labels(terms)
  vars <- get_variables(terms)
  if (length(vars) > 0L && !all(vars %in% tags)) {
    stop("the specified terms contain labels that are not found in 'x'")
  }
  ord <- attr(terms, "order")
  mts <- unique(labs[ord == 1L])
  its <- unique(labs[ord == 2L])
  p <- length(mts)
  q <- length(its)
  if (p + q == 0L && !fit.intercept) {
    stop(
      "the specified terms contain no valid term labels up to second-order:",
      "\nset 'fit.intercept = TRUE' to allow an intercept-only model"
    )
  }
  verbose(text = paste0(
    "'terms' include ", if (p) add_s(p, "main effect"),
    if (p > 0L && q > 0L) " and ", if (q) add_s(q, "interaction")
  ), verbosity, 3L)

  if (is.na(k[1L])) {
    k[1L] <- min(25L, max(2L, if (lambda > 0) 25L else n %/% (p + q)))
  }
  if (is.na(k[2L])) {
    k[2L] <- min(5L, max(2L, if (lambda > 0) 5L else floor(sqrt(n / (p + q)))))
  }
  verbose(text = paste0(collapse = "",
                        c("'k' is set to ", if (p > 0L) c(k[1L], " for main effects"),
                          if (p > 0L && q > 0L) " and ", if (q > 0L) c(k[2L], " for interactions"))
  ), verbosity, 3L)

  me <- (p > 0L)
  ie <- (q > 0L)
  menc <- mmat <- ienc <- imat <- list()
  enc_weights <- if (weighted.encoding) weights else NULL
  if (me) {
    for (tag in mts) {
      menc[[tag]] <- make_encoder(
        x = x[[tag]], k = k[1L], type = type[1L], split = split,
        digits = digits, lump = lump, others = others, sep = sep,
        weights = enc_weights,
        spec = encoders$main.effects[[tag]] %||% encoders[[tag]], tag = tag
      )
      if (save.memory < 1L) mmat[[tag]] <- menc[[tag]]$encode(x[[tag]])
    }
  }
  if (ie) {
    for (tag in get_variables(its)) {
      ienc[[tag]] <- make_encoder(
        x = x[[tag]], k = k[2L], type = type[2L], split = split,
        digits = digits, lump = lump, others = others, sep = sep,
        weights = enc_weights,
        spec = encoders$interactions[[tag]] %||% encoders[[tag]], tag = tag
      )
      if (save.memory < 2L) imat[[tag]] <- ienc[[tag]]$encode(x[[tag]])
    }
  }

  fiti <- as.integer(fit.intercept)
  u <- v <- s <- 0L
  mlen <- mcumlen <- ilen <- plen <- pcumlen <- NULL
  if (me) {
    mlen <- vapply(menc, function(e) e$n, integer(1L))
    mcumlen <- structure(cumsum(c(0L, mlen)), names = c(mts, NA_character_))
    u <- mcumlen[length(mcumlen)]
  }
  if (ie) {
    ilen <- vapply(ienc, function(e) e$n, integer(1L))
    plen <- structure(integer(q), names = its)
    for (i in seq_len(q)) {
      itag <- get_variables(its[i])
      plen[i] <- ilen[itag[1L]] * ilen[itag[2L]]
      s <- s + ilen[itag[1L]] + ilen[itag[2L]]
    }
    pcumlen <- structure(cumsum(c(0L, plen)), names = c(its, NA_character_))
    v <- pcumlen[length(pcumlen)]
  }
  npar <- fiti + u + v
  ncon <- p + s
  delt <- rep.int(1, npar)
  vnil <- logical(npar)
  lnil <- list()
  lreg <- list()

  for (i in seq_len(p)) {
    mtag <- mts[i]
    cols <- fiti + mcumlen[i] + seq_len(mlen[i])
    vsum <- colSums((mmat[[mtag]] %||% menc[[mtag]]$encode(x[[mtag]])) * weights)
    is_nil <- vsum <= nil
    vnil[cols] <- is_nil
    vsum[is_nil] <- 0
    delt[cols] <- vsum
    ordr <- orvs[mtag]
    for (j in seq_len(mlen[i])) {
      m <- cols[j]
      if (vnil[m]) {
        lnil[[length(lnil) + 1L]] <- c(
          m,
          if (ordr && j > 1L) m - 1L,
          if (ordr && j < mlen[[i]]) m + 1L
        )
      } else if (lambda > 0 && ordr) {
        lreg[[length(lreg) + 1L]] <- c(
          m,
          if (j > 1L) m - 1L,
          if (j < mlen[[i]]) m + 1L
        )
      }
    }
  }
  for (i in seq_len(q)) {
    itag <- get_variables(its[i])
    cols <- fiti + u + pcumlen[i] + seq_len(plen[i])
    mat1 <- imat[[itag[1L]]] %||% ienc[[itag[1L]]]$encode(x[[itag[1L]]])
    mat2 <- imat[[itag[2L]]] %||% ienc[[itag[2L]]]$encode(x[[itag[2L]]])
    vsum <- as.numeric(crossprod(mat1, mat2 * weights))
    mat1 <- mat2 <- NULL
    is_nil <- vsum <= nil
    vnil[cols] <- is_nil
    vsum[is_nil] <- 0
    delt[cols] <- vsum
    nval <- ilen[itag]
    ordr <- orvs[itag]
    for (j in seq_len(plen[i])) {
      m <- cols[j]
      posn <- c((j - 1L) %% nval[1L] + 1L, (j - 1L) %/% nval[1L] + 1L)
      if (vnil[m]) {
        lnil[[length(lnil) + 1L]] <- c(
          m,
          if (ordr[1L] && posn[1L] > 1L) m - 1L,
          if (ordr[1L] && posn[1L] < nval[1L]) m + 1L,
          if (ordr[2L] && posn[2L] > 1L) m - nval[1L],
          if (ordr[2L] && posn[2L] < nval[2L]) m + nval[1L]
        )
      } else if (lambda > 0) {
        if (ordr[1L]) {
          lreg[[length(lreg) + 1L]] <- c(
            m,
            if (posn[1L] > 1L) m - 1L,
            if (posn[1L] < nval[1L]) m + 1L
          )
        }
        if (ordr[2L]) {
          lreg[[length(lreg) + 1L]] <- c(
            m,
            if (posn[2L] > 1L) m - nval[1L],
            if (posn[2L] < nval[2L]) m + nval[1L]
          )
        }
      }
    }
  }

  nreg <- length(lreg)
  nnil <- length(lnil)
  nfin <- n + nreg + (if (mode == 1L) ncon + nnil else 0L)
  nelements <- nfin * npar
  if (!is.null(max.nelements) && nelements > max.nelements) {
    title <- sprintf("estimated design matrix size: %.2f GB (%.0f elements)",
                     nelements * 8 / (1024 ^ 3), nelements)
    if (verbosity < 1L || !interactive()) {
      stop(title, call. = FALSE)
    }
    sel <- try(utils::select.list(c("exit", "continue"), title = title), silent = TRUE)
    if (inherits(sel, "try-error") || !nzchar(sel) || sel == "exit") {
      stop("number of elements in the design matrix exceeded 'max.nelements'", call. = FALSE)
    }
  }
  verbose(paste0(
    npar, " parameters", if (nnil > 0L) paste0(" (", nnil, " inestimable)"),
    ", ", n, " observations, ", ncon, " centering constraints",
    if (nreg > 0L) paste0(", ", nreg, " smoothing constraints")
  ), verbosity, 3L, FALSE)

  list(
    terms = terms, mts = mts, its = its, p = p, q = q, me = me, ie = ie,
    menc = menc, mmat = mmat, ienc = ienc, imat = imat,
    mlen = mlen, mcumlen = mcumlen, ilen = ilen, plen = plen, pcumlen = pcumlen,
    fiti = fiti, u = u, v = v, npar = npar, ncon = ncon, nreg = nreg,
    nnil = nnil, nfin = nfin, nelements = nelements,
    delt = delt, vnil = vnil, lnil = lnil, lreg = lreg
  )
}


build_design_matrix <- function(
    x, y, weights, meta, fit.intercept, mode, lambda, kappa,
    weighted.norm, penalize.edges
) {
  n <- nrow(x)
  ntargets <- ncol(y)
  nfin <- meta$nfin
  npar <- meta$npar
  nreg <- meta$nreg
  ncon <- meta$ncon
  nnil <- meta$nnil
  fiti <- meta$fiti
  delt <- meta$delt
  vnil <- meta$vnil
  rw <- sqrt(weights)
  rk <- sqrt(kappa)
  rl <- sqrt(lambda)

  X <- matrix(0, nrow = nfin, ncol = npar)
  M <- if (mode == 2L) matrix(0, nrow = ncon + nnil, ncol = npar) else NULL
  Y <- matrix(0, nrow = nfin, ncol = ntargets)

  intercept <- NULL
  if (fit.intercept) {
    delt[1L] <- n
    X[seq_len(n), 1L] <- if (weighted.norm) rw / sqrt(n) else rw
    Y[seq_len(n), ] <- y * rw
  } else {
    intercept <- colSums(y * weights) / sum(weights)
    Y[seq_len(n), ] <- sweep(y, 2L, intercept, "-") * rw
  }

  for (i in seq_len(meta$p)) {
    mtag <- meta$mts[i]
    cols <- fiti + meta$mcumlen[i] + seq_len(meta$mlen[i])
    vfil <- !vnil[cols]
    if (!any(vfil)) next
    Xsub <- meta$mmat[[mtag]] %||% meta$menc[[mtag]]$encode(x[[mtag]])
    X[seq_len(n), cols[vfil]] <- if (weighted.norm) {
      sweep(Xsub[, vfil, drop = FALSE], 2L, sqrt(delt[cols[vfil]]), "/") * rw
    } else {
      Xsub[, vfil, drop = FALSE] * rw
    }
    Xsub <- NULL
    cvec <- if (weighted.norm) sqrt(delt[cols[vfil]]) else delt[cols[vfil]]
    if (mode == 1L) {
      X[n + nreg + i, cols[vfil]] <- cvec * rk
    } else {
      M[i, cols[vfil]] <- cvec
    }
  }

  offs <- meta$p
  for (i in seq_len(meta$q)) {
    itag <- get_variables(meta$its[i])
    cols <- fiti + meta$u + meta$pcumlen[i] + seq_len(meta$plen[i])
    vfil <- !vnil[cols]
    nval <- meta$ilen[itag]
    if (any(vfil)) {
      mat1 <- meta$imat[[itag[1L]]] %||% meta$ienc[[itag[1L]]]$encode(x[[itag[1L]]])
      mat2 <- meta$imat[[itag[2L]]] %||% meta$ienc[[itag[2L]]]$encode(x[[itag[2L]]])
      idx1 <- rep(seq_len(nval[1L]), times = nval[2L])[vfil]
      idx2 <- rep(seq_len(nval[2L]), each = nval[1L])[vfil]
      Xsub <- mat1[, idx1, drop = FALSE] * mat2[, idx2, drop = FALSE]
      mat1 <- mat2 <- NULL
      X[seq_len(n), cols[vfil]] <- if (weighted.norm) {
        sweep(Xsub, 2L, sqrt(delt[cols[vfil]]), "/") * rw
      } else {
        Xsub * rw
      }
      Xsub <- NULL
      for (j in seq_len(meta$plen[i])) {
        m <- cols[j]
        if (vnil[m]) next
        posn <- c((j - 1L) %% nval[1L] + 1L, (j - 1L) %/% nval[1L] + 1L)
        cval <- if (weighted.norm) sqrt(delt[m]) else delt[m]
        if (mode == 1L) {
          X[offs + n + nreg + posn[1L], m] <- cval * rk
          X[offs + n + nreg + nval[1L] + posn[2L], m] <- cval * rk
        } else {
          M[offs + posn[1L], m] <- cval
          M[offs + nval[1L] + posn[2L], m] <- cval
        }
      }
    }
    offs <- offs + nval[1L] + nval[2L]
  }

  if (nreg > 0L) {
    min.neighbors <- if (penalize.edges) 1L else 2L
    for (i in seq_len(nreg)) {
      m <- meta$lreg[[i]][1L]
      a <- meta$lreg[[i]][-1L]
      a <- a[!vnil[a]]
      if (length(a) < min.neighbors) next
      if (weighted.norm) {
        X[n + i, m] <- length(a) * rl
        X[n + i, a] <- (-1) * sqrt(delt[m]) / sqrt(delt[a]) * rl
      } else {
        X[n + i, m] <- length(a) * sqrt(delt[m]) * rl
        X[n + i, a] <- (-1) * sqrt(delt[m]) * rl
      }
    }
  }

  if (nnil > 0L) {
    for (i in seq_len(nnil)) {
      if (mode == 1L) {
        X[n + nreg + ncon + i, meta$lnil[[i]][1L]] <- rk
      } else {
        M[ncon + i, meta$lnil[[i]][1L]] <- 1
      }
    }
  }

  list(X = X, Y = Y, M = M, delt = delt, intercept = intercept)
}


solve_mid <- function(
    dm, meta, mode, method, singular.ok, weighted.norm,
    interpolation, max.niterations, fit.intercept, kappa, nil, tol, verbosity
) {
  verbose("least squares estimation initiated", verbosity, 2L, FALSE)
  npar <- meta$npar
  ncon <- meta$ncon
  nreg <- meta$nreg
  n <- nrow(dm$Y) - nreg - (if (mode == 1L) ncon + meta$nnil else 0L)
  rk <- sqrt(kappa)

  if (mode == 1L) {
    r <- 0L
    z <- try(solve_ols(dm$X, dm$Y, tol, method), silent = verbosity < 3L)
    if (inherits(z, "try-error")) {
      stop("failed to solve the least squares problem")
    }
    if (!is.null(attr(z, "message"))) {
      verbose(attr(z, "message"), verbosity, 2L, FALSE)
    }
    beta <- as.matrix(z$coefficients)
    beta[is.na(beta)] <- 0
    if (is.null(z$residuals)) {
      verbose(sprintf(
        "strict centering cannot be verified because the solver '%s' does not return residuals",
        attr(z, "method")
      ), verbosity, 1L, FALSE)
    } else if (ncon > 0L) {
      crsd <- as.matrix(z$residuals)[n + nreg + seq_len(ncon), , drop = FALSE]
      maxerr <- max(abs(crsd), 0) / (rk * n)
      if (maxerr > nil) {
        verbose(paste0("not strictly centered: max absolute average effect = ",
                       format(maxerr, digits = 6L)),
                verbosity, level = 1L)
      }
    }
  } else {
    Msvd <- svd(dm$M, nv = npar)
    r <- sum(Msvd$d > tol)
    if (r == dim(Msvd$v)[2L]) {
      stop("no coefficients to evaluate found")
    }
    vr <- Msvd$v[, (r + 1L):npar, drop = FALSE]
    z <- try(solve_ols(dm$X %*% vr, dm$Y, tol, method), silent = verbosity < 3L)
    if (inherits(z, "try-error")) {
      stop("failed to solve the least squares problem")
    }
    if (!is.null(attr(z, "message"))) {
      verbose(attr(z, "message"), verbosity, 2L, FALSE)
    }
    coef <- as.matrix(z$coefficients)
    coef[is.na(coef)] <- 0
    beta <- as.matrix(vr %*% coef)
  }

  if (is.null(z$rank) || anyNA(z$rank)) {
    if (!singular.ok) {
      verbose(sprintf(
        "singular fits cannot be detected because the solver '%s' does not return rank information",
        attr(z, "method")
      ), verbosity, 1L, FALSE)
    }
  } else if (z$rank < npar - r) {
    if (!singular.ok) {
      title <- "singular fit encountered"
      if (verbosity < 1L || !interactive()) {
        stop(title, call. = FALSE)
      }
      sel <- try(utils::select.list(c("exit", "continue"), title = title), silent = TRUE)
      if (inherits(sel, "try-error") || !nzchar(sel) || sel == "exit") {
        stop("execution halted: singular fit encountered", call. = FALSE)
      }
    }
    verbose("singular fit encountered", verbosity, level = 1L)
  }

  lnil <- meta$lnil[vapply(meta$lnil, length, 0L) > 1L]
  nnil <- length(lnil)
  if (weighted.norm) {
    beta[!meta$vnil] <- beta[!meta$vnil] / sqrt(dm$delt[!meta$vnil])
  }
  if (!(interpolation == "none" || isFALSE(interpolation)) && nnil > 0L) {
    verbose("interpolating inestimable parameters ...", verbosity, 3L)
    if (interpolation == "iterative" || isTRUE(interpolation)) {
      nils <- as.integer(vapply(lnil, `[`, numeric(1L), 1L))
      adjs <- lapply(lnil, `[`, -1L)
      pnts <- cumsum(c(1L, lengths(adjs)))
      res <- LaplacianSmoothing(
        beta, nils, as.integer(unlist(adjs)), pnts, tol, max.niterations
      )
      verbose(paste0(
        "interpolation ", if (res$converged) "converged" else "stopped",
        " after ", res$n_iter, " iterations"
      ), verbosity, 3L)
      beta <- res$coefficients
    } else {
      B <- diag(1, npar)
      for (i in seq_len(nnil)) {
        a <- lnil[[i]][-1L]
        m <- lnil[[i]][1L]
        B[m, a] <- -1
        B[m, m] <- length(a)
      }
      beta <- as.matrix(solve_ols(B, beta, tol, method)$coefficients)
    }
    beta[is.na(beta)] <- 0
  }
  beta[abs(beta) <= nil] <- 0
  verbose("least squares estimation completed", verbosity, 2L, FALSE)
  list(beta = beta, method = attr(z, "method"))
}


assemble_object <- function(
    object, cl, prep, meta, fit, delt, intercept, na.action, verbosity
) {
  x <- prep$x
  y <- prep$y
  yres <- prep$yres
  weights <- prep$weights
  link <- prep$link
  n <- prep$n
  ntargets <- prep$ntargets
  naai <- prep$naai
  beta <- fit$beta

  if (is.null(intercept)) {
    intercept <- structure(beta[1L, ], names = colnames(y))
  } else {
    intercept <- attract(intercept, 0)
  }
  lp <- matrix(rep(intercept, each = n), nrow = n, ncol = ntargets,
               dimnames = list(NULL, colnames(y)))

  ret.main.effects <- list()
  for (i in seq_len(meta$p)) {
    mtag <- meta$mts[i]
    dat <- meta$menc[[mtag]]$frame
    cols <- meta$fiti + meta$mcumlen[i] + seq_len(meta$mlen[i])
    dat$.density <- delt[cols] / n
    bmat <- beta[cols, , drop = FALSE]
    dat$.effect <- if (ntargets == 1L) {
      as.numeric(bmat)
    } else {
      I(structure(bmat, dimnames = list(NULL, colnames(y))))
    }
    ret.main.effects[[mtag]] <- dat
    lp <- lp + (meta$mmat[[mtag]] %||% meta$menc[[mtag]]$encode(x[[mtag]])) %*% bmat
  }

  ret.interactions <- list()
  for (i in seq_len(meta$q)) {
    itag <- get_variables(meta$its[i])
    dat <- interaction_frame(meta$ienc[[itag[1L]]]$frame, meta$ienc[[itag[2L]]]$frame)
    cols <- meta$fiti + meta$u + meta$pcumlen[i] + seq_len(meta$plen[i])
    dat$.density <- delt[cols] / n
    bmat <- beta[cols, , drop = FALSE]
    dat$.effect <- if (ntargets == 1L) {
      as.numeric(bmat)
    } else {
      I(structure(bmat, dimnames = list(NULL, colnames(y))))
    }
    ret.interactions[[meta$its[i]]] <- dat
    mat1 <- meta$imat[[itag[1L]]] %||% meta$ienc[[itag[1L]]]$encode(x[[itag[1L]]])
    mat2 <- meta$imat[[itag[2L]]] %||% meta$ienc[[itag[2L]]]$encode(x[[itag[2L]]])
    n1 <- meta$ilen[itag[1L]]
    n2 <- meta$ilen[itag[2L]]
    if (ntargets == 1L) {
      W <- matrix(beta[cols], nrow = n1, ncol = n2)
      lp <- lp + rowSums((mat1 %*% W) * mat2)
    } else {
      mat3 <- mat1[, rep(seq_len(n1), times = n2), drop = FALSE] *
        mat2[, rep(seq_len(n2), each = n1), drop = FALSE]
      lp <- lp + (mat3 %*% bmat)
      mat3 <- NULL
    }
    mat1 <- mat2 <- NULL
  }

  obj <- list()
  class(obj) <- if (ntargets == 1L) "mid" else c("mids", "midrib")
  obj$model.class <- attr(object, "class")
  obj$call <- cl
  obj$terms <- meta$terms
  obj$link <- link
  obj$intercept <- if (ntargets == 1L) as.numeric(intercept) else intercept
  obj$encoders <- list()
  if (meta$me) {
    obj$main.effects <- ret.main.effects
    obj$encoders[["main.effects"]] <- meta$menc
  }
  if (meta$ie) {
    obj$interactions <- ret.interactions
    obj$encoders[["interactions"]] <- meta$ienc
  }
  obj$weights <- weights
  obj$method <- fit$method
  obj$fitted.values <- if (ntargets == 1L) as.numeric(lp) else lp
  residuals <- y - lp
  obj$residuals <- if (ntargets == 1L) as.numeric(residuals) else residuals
  tss <- colSums(sweep(y, 2L, intercept, "-") ^ 2 * weights)
  rss <- colSums(residuals ^ 2 * weights)
  uir <- attract(rss / tss, 0)
  verbose(paste0("uninterpreted variation ratio: ", examples(uir)), verbosity, 3L)
  obj$ratio <- uir

  if (!is.null(link)) {
    obj$linear.predictors <- obj$fitted.values
    fitted <- link$linkinv(lp)
    obj$fitted.values <- if (ntargets == 1L) as.numeric(fitted) else fitted
    resres <- yres - fitted
    obj$response.residuals <- if (ntargets == 1L) as.numeric(resres) else resres
    mu <- colSums(yres * weights) / sum(weights)
    tss <- colSums(sweep(yres, 2L, mu, "-") ^ 2 * weights)
    rss <- colSums(resres ^ 2 * weights)
    uir <- attract(rss / tss, 0)
    verbose(paste0("uninterpreted variation ratio (response): ", examples(uir)), verbosity, 3L)
    obj$ratio <- if (ntargets == 1L) {
      c(working = obj$ratio, response = uir)
    } else {
      rbind(working = obj$ratio, response = uir)
    }
  }
  if (length(naai$ids) < naai$n.init) {
    naacl <- attr(attr(do.call(na.action, list(NA)), "na.action"), "class")
    obj$na.action <- structure((1L:naai$n.init)[-naai$ids], class = naacl)
  }
  obj
}


#' Internal OLS Solver
#'
#' @description
#' An OLS solver utilized in \code{\link{interpret}()} supporting multiple computational methods.
#' It utilizes \code{\link[RcppEigen]{fastLmPure}()} for single-response variables to optimize speed independently of the environment, and uses base BLAS computations for multiple responses.
#'
#' @param x a design matrix.
#' @param y a numeric vector or matrix of the response variable.
#' @param tol the tolerance used in solvers. Defaults to \code{1e-7}.
#' @param method an integer or character string specifying the computational method. If a character string (e.g., \code{"FUNC"}) is passed, the function registered in the global option \code{"midr.solver.FUNC"} is used.
#' @param ... additional arguments.
#'
#' @returns
#' \code{solve_ols()} returns a list containing at least the \code{coefficients}, and potentially \code{residuals} or \code{rank} depending on the computational method.
#'
#' @keywords internal
#'
solve_ols <- function(x, y, tol = 1e-7, method = 0L, ...) {
  map <- c(
    qr = 0L, unpivoted.qr = 1L, llt = 2L, ldlt = 3L, svd = 4L, eigen = 5L
  )
  custom.solver <- NULL
  method <- method[1L]
  if (is.character(method)) {
    name <- method
    method <- if (any(name == names(map))) map[[name]] else -1L
    custom.solver <- getOption(paste0("midr.solver.", name))
  } else if (is.numeric(method)) {
    method <- as.integer(method)
    name <- names(map)[match(method, map)]
    if (is.na(name)) name <- as.character(method)
  } else {
    name <- "qr"
    method <- -1L
  }
  fmsg <- "falling back to 'qr' with stats::.lm.fit"
  if (!is.null(custom.solver) && is.function(custom.solver)) {
    z <- tryCatch(custom.solver(x, y), error = function(e) e)
    if (inherits(z, "error")) {
      emsg <- gsub("\n", " ", conditionMessage(z))
      msg <- sprintf("custom solver '%s' failed (%s): %s", name, emsg, fmsg)
      z <- stats::.lm.fit(x, y, tol = tol)
      return(structure(z, method = "qr", message = msg))
    } else if (is.null(z$coefficients)) {
      msg <- sprintf("custom solver '%s' did not return 'coefficients': %s",
                     name, fmsg)
      z <- stats::.lm.fit(x, y, tol = tol)
      return(structure(z, method = "qr", message = msg))
    } else {
      msg <- sprintf("custom solver '%s' is used", name)
      return(structure(z, method = name, message = msg))
    }
  }
  if (NCOL(y) == 1L && any(method == 0L:5L)) {
    return(structure(RcppEigen::fastLmPure(x, y, method), method = name))
  }
  z <- tryCatch(switch(
    name,
    "qr" = {
      stats::.lm.fit(x, y, tol = tol)
    }, "llt" = {
      XtX <- crossprod(x)
      XtY <- crossprod(x, y)
      R <- chol(XtX)
      coef <- backsolve(R, forwardsolve(t(R), XtY))
      list(coefficients = coef, residuals = y - x %*% coef)
    }, "svd" = {
      s <- svd(x)
      dinv <- numeric(length(s$d))
      pos <- s$d > tol * s$d[1L]
      dinv[pos] <- 1 / s$d[pos]
      coef <- s$v %*% (dinv * crossprod(s$u, y))
      list(coefficients = coef, residuals = y - x %*% coef, rank = sum(pos))
    },
    NULL
  ), error = function(e) e)
  if (is.null(z)) {
    mmsg <- if (NCOL(y) > 1L) " for matrix" else ""
    msg <- sprintf("method '%s' is not supported%s: %s", name, mmsg, fmsg)
    z <- stats::.lm.fit(x, y, tol = tol)
    return(structure(z, method = "qr", message = msg))
  }
  if (inherits(z, "error")) {
    emsg <- gsub("\n", " ", conditionMessage(z))
    msg <- sprintf("method '%s' failed (%s): %s", name, emsg, fmsg)
    z <- stats::.lm.fit(x, y, tol = tol)
    return(structure(z, method = "qr", message = msg))
  }
  return(structure(z, method = name))
}
