#' Extract Term Response from a MID Model
#'
#' @export term_response
term_response <- function(object, ...)
UseMethod("term_response")


#' @exportS3Method midr::term_response
term_response.mid <- function(
    object, term, data = NULL, intercept = FALSE, main.effects = FALSE,
    lumped = TRUE, resolution = c(100L, 100L), ...
) {
  mlab <- match.labels(term, term.labels(object), single = TRUE)
  if (is.na(mlab)) stop(sprintf("term '%s' is not found in 'object'.", term), call. = FALSE)
  tags <- get.variables(mlab)
  ie <- (length(tags) > 1L)
  lumped <- isTRUE(lumped) && isFALSE(main.effects)
  ints <- if (intercept) get.intercept(object) else 0

  # 1. 評価グリッド (grid) の構築と効果の計算
  if (!ie) {
    enc <- object$encoders$main.effects[[mlab]]
    if (enc$type != "factor" || lumped) {
      grid <- stats::na.omit(object$main.effects[[mlab]])
    } else {
      olvs <- enc$envir$olvs
      grid <- data.frame(factor(olvs, levels = olvs))
      colnames(grid) <- mlab
      grid$.effect <- term.effect(object, mlab, grid)
    }
    if (intercept) grid$.effect <- grid$.effect + ints
    meta <- list(enc_types = enc$type)
  } else {
    encs <- list(
      object$encoders$interactions[[tags[1L]]],
      object$encoders$interactions[[tags[2L]]]
    )
    ms <- if (length(resolution) == 1L) c(resolution, resolution) else resolution[1L:2L]
    xy <- lat <- lab <- vector("list", 2L)
    for (i in 1L:2L) {
      if (encs[[i]]$type != "factor" || lumped) {
        frm <- encs[[i]]$frame
      } else {
        olvs <- encs[[i]]$envir$olvs
        frm <- data.frame(factor(olvs, levels = olvs))
      }
      if (encs[[i]]$type == "factor") {
        xy[[i]] <- rep(frm[[1L]], each = 2L)
        ms[i] <- length(xy[[i]])
        lab[[i]] <- frm[[1L]]
        lat[[i]] <- seq_along(lab[[i]])
      } else {
        cns <- paste0(tags[i], c("_min", "_max"))
        xy[[i]] <- seq(min(frm[[cns[1L]]]), max(frm[[cns[2L]]]), length.out = ms[i])
      }
    }
    grid <- data.frame(
      rep(xy[[1L]], times = ms[2L]),
      rep(xy[[2L]], each = ms[1L])
    )
    colnames(grid) <- tags
    z <- term.effect(object, mlab, grid) + ints
    if (main.effects) {
      z <- z + term.effect(object, tags[1L], grid) + term.effect(object, tags[2L], grid)
    }
    grid$.effect <- z
    meta <- list(
      enc_types = c(encs[[1L]]$type, encs[[2L]]$type),
      xy = xy, ms = ms, lat = lat, lab = lab
    )
  }

  # 2. 観測データ (data) の効果計算（指定された場合のみ）
  obs_data <- NULL
  if (!is.null(data)) {
    preds <- stats::predict(
      object, data, terms = unique(c(tags, mlab)),
      type = "terms", na.action = "na.pass"
    )
    obs_data <- model.reframe(object, data)[, tags, drop = FALSE]
    obs_terms <- if (ie && main.effects) c(mlab, tags) else mlab
    obs_data$.effect <- rowSums(preds[, obs_terms, drop = FALSE]) + ints
    # Jitter用のtransform処理のためにエンコーダー参照を保持
    meta$transforms <- if (!ie) list(enc$transform) else list(encs[[1L]]$transform, encs[[2L]]$transform)
  }

  # 3. データフレーム本体に属性としてメタデータ・観測データを付与して返す
  structure(
    grid,
    term = mlab,
    tags = tags,
    intercept = intercept,
    middle = ints,
    main.effects = main.effects,
    lumped = lumped,
    obs_data = obs_data,
    meta = meta,
    class = c("term_response", "data.frame")
  )
}
