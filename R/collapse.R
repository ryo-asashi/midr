#' @export
#'
collapse <- function(object, ...)
UseMethod("collapse")


#' @exportS3Method midr::collapse
collapse.mid <- function(
    object, axis = NULL,
    method = c("auto", "identity", "marginal", "slice", "shap"),
    background = NULL, ...
) {
  method <- match.arg(method)
  terms   <- as_terms(object)
  active_vars <- get_variables(terms)
  active_labs <- get_labels(terms)
  axis <- if (inherits(axis, "formula") || inherits(axis, "terms")) {
    get_variables(axis)
  } else {
    axis %||% active_vars
  }
  nuisance_vars <- setdiff(active_vars, axis)
  if (method == "auto") {
    method <- if (length(nuisance_vars) == 0L) "identity" else "marginal"
  }
  structure(
    list(
      model         = object,
      terms         = terms,
      axis          = axis,
      nuisance_vars = nuisance_vars,
      method        = method,
      background    = background
    ),
    class = unique(c("collapsed", class(object)))
  )
}


# ==============================================================================
# 4. evaluate(): Target ドメイン（Grid または実データ）上での評価実行
# ==============================================================================

#' 意図された定義域（Grid）を encoders 等から生成するヘルパー
#' @export
make_axis_grid <- function(mid_obj, axis, n = 100) {
  # ※ 実装時は mid_obj$encoders[[v]] の knots や levels から正確な定義域を取得する
  # ここでは概念実装としてプレースホルダを記述
  grid_list <- lapply(axis, function(v) {
    enc <- mid_obj$encoders[[v]]
    if (!is.null(enc$levels)) {
      enc$levels # カテゴリ変数の意図された全水準
    } else if (!is.null(enc$range)) {
      seq(enc$range[1], enc$range[2], length.out = n) # 連続変数の定義域メッシュ
    } else {
      stop(sprintf("Domain information for variable '%s' not found in encoders.", v))
    }
  })
  names(grid_list) <- axis
  expand.grid(grid_list, stringsAsFactors = FALSE)
}

#' @export
evaluate <- function(object, ...) {
  UseMethod("evaluate")
}

# submodel から直接 evaluate() を呼んだ場合は、自動的に collapse(axis = active_vars) を挟む
#' @exportS3Method midr::evaluate
evaluate.mid <- function(object, target = "grid", ...) {
  active_terms <- attr(object, "active_terms")
  if (is.null(active_terms)) {
    active_terms <- c(names(object$main.effects), names(object$interactions))
  }
  active_vars <- unique(unlist(.parse_terms(active_terms)))
  collapsed <- collapse.mid(object, axis = active_vars, method = "identity")
  evaluate.mid_collapsed(collapsed, target = target, ...)
}

#' @exportS3Method midr::evaluate
evaluate.collapsed <- function(object, target = "grid", grid_n = 100, contrast = NULL, ...) {
  mod    <- object$model
  axis   <- object$axis
  method <- object$method
  bg     <- object$background
  terms  <- object$active_terms

  # 1. Target ドメイン（評価点集合）の解決
  if (identical(target, "grid")) {
    if (method == "shap") {
      stop("`method = 'shap'` requires actual observation points in `target` (a data.frame), not `'grid'`.")
    }
    target_df <- make_axis_grid(mod, axis = axis, n = grid_n)
    domain_type <- "grid"
  } else if (is.data.frame(target)) {
    target_df <- target
    domain_type <- "data"
  } else {
    stop("`target` must be `'grid'` or a data.frame.")
  }

  # 2. Method ごとの評価アルゴリズム（加法モデルの構造を活用した高速計算）
  if (method == "identity") {
    # 余分な変数がないため、Target 上でそのまま予測値を計算 (O(N_target))
    val <- predict(mod, newdata = target_df, terms = terms, type = "response")
    out_df <- target_df[, axis, drop = FALSE]
    out_df$.value <- as.numeric(val)

  } else if (method == "marginal") {
    # 各項ごとに必要な変数だけを見て周辺化する（全変数の直積を作らない最適化が可能）
    # シンプルな汎用実装：Target の axis 列と Background の nuisance 列を結合して平均
    n_tgt <- nrow(target_df)
    n_bg  <- nrow(bg)

    # axis 以外の列を bg から補完して評価
    val <- vapply(seq_len(n_tgt), function(i) {
      eval_chunk <- bg
      for (v in axis) {
        eval_chunk[[v]] <- target_df[[v]][i]
      }
      mean(predict(mod, newdata = eval_chunk, terms = terms, type = "response"), na.rm = TRUE)
    }, numeric(1))

    out_df <- target_df[, axis, drop = FALSE]
    out_df$.value <- val

  } else if (method == "slice") {
    # ICE相当: Background の各個体 (id) ごとに曲線束を生成
    n_tgt <- nrow(target_df)
    n_bg  <- nrow(bg)

    res_list <- lapply(seq_len(n_bg), function(b_idx) {
      eval_chunk <- bg[rep(b_idx, n_tgt), , drop = FALSE]
      for (v in axis) {
        eval_chunk[[v]] <- target_df[[v]]
      }
      df_slice <- target_df[, axis, drop = FALSE]
      df_slice$.id <- b_idx
      df_slice$.value <- as.numeric(predict(mod, newdata = eval_chunk, terms = terms, type = "response"))
      df_slice
    })
    out_df <- do.call(rbind, res_list)

  } else if (method == "shap") {
    # 加法モデルにおける閉形式 Shapley 配分:
    # axis の変数を含む各項 S について、項の評価値を |S| (次数) で割って足し合わせる
    term_preds <- predict(mod, newdata = target_df, terms = terms, type = "terms")
    parsed <- .parse_terms(terms)

    # 各項が axis 内の変数をいくつ含んでいるか / 項全体の次数はいくつかで重み付け
    weights <- vapply(parsed, function(vars_in_term) {
      n_in_axis <- sum(vars_in_term %in% axis)
      n_total   <- length(vars_in_term)
      n_in_axis / n_total
    }, numeric(1))

    # 重み付き和を計算 (N_target x N_terms) %*% (N_terms x 1)
    val <- as.numeric(as.matrix(term_preds[, terms, drop = FALSE]) %*% weights)
    out_df <- target_df[, axis, drop = FALSE]
    out_df$.value <- val
  }

  # 3. Contrast（基準点との差分）が指定されている場合の減算処理
  if (!is.null(contrast)) {
    # contrast には基準となる1行の data.frame 等を渡す想定
    ref_eval <- evaluate.mid_collapsed(object, target = contrast, contrast = NULL)
    out_df$.value <- out_df$.value - ref_eval$data$.value[1]
  }

  structure(
    list(
      data        = out_df,
      axis        = axis,
      method      = method,
      domain_type = domain_type,
      terms       = terms
    ),
    class = "mid_evaluated"
  )
}


# ==============================================================================
# 5. summarize_effect(): 評価結果の集約（Functional vs Scalar Importance）
# ==============================================================================

#' @export
summarize_effect <- function(object, fn = c("identity", "mean_abs", "sd", "mean", "range"), ...) {
  stopifnot(inherits(object, "mid_evaluated"))
  fn <- match.arg(fn)

  vals <- object$data$.value

  if (fn == "identity") {
    # 集約せず、曲線・曲面（または個体別SHAP値）のテーブルをそのまま返す
    return(object)
  }

  # スカラー値（Importance 等）への縮約
  summary_val <- switch(
    fn,
    mean_abs = mean(abs(vals), na.rm = TRUE),
    sd       = stats::sd(vals, na.rm = TRUE),
    mean     = mean(vals, na.rm = TRUE),
    range    = diff(range(vals, na.rm = TRUE))
  )

  data.frame(
    axis   = paste(object$axis, collapse = ":"),
    terms  = paste(object$terms, collapse = " + "),
    method = object$method,
    metric = fn,
    value  = summary_val,
    stringsAsFactors = FALSE
  )
}
