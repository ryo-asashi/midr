test_that("colortheme() creates themes from color vectors, palettes, and ramps", {
  # 1. Color vector kernel (sequential by default)
  cols <- c("#003f5c", "#bc5090", "#ffa600")
  ct_vec <- colortheme(cols, name = "custom_seq")
  expect_s3_class(ct_vec, "colortheme")
  expect_equal(ct_vec$type, "sequential")
  expect_equal(ct_vec$kernel.class, "color")
  expect_length(ct_vec$palette(5L), 5L)
  expect_equal(ct_vec$palette(0L), character(0L))
  expect_length(ct_vec$ramp(c(0, 0.5, 1)), 3L)

  # Color vector with alpha
  cols_alpha <- c("#003f5c80", "#ffa60080")
  ct_alpha <- colortheme(cols_alpha)
  expect_true(all(grepl("^#[0-9A-Fa-f]{8}$", ct_alpha$ramp(c(0, 1)))))

  # 2. Palette function kernel
  ct_pal <- colortheme(grDevices::rainbow, type = "qualitative")
  expect_equal(ct_pal$kernel.class, "palette")
  expect_equal(ct_pal$type, "qualitative")
  expect_length(ct_pal$palette(4L), 4L)
  expect_length(ct_pal$ramp(c(0, 0.5, 1)), 3L)

  # 3. Ramp function kernel
  my_ramp <- function(x) grDevices::rgb(x, 0, 1 - x)
  ct_ramp <- colortheme(my_ramp, type = "diverging")
  expect_equal(ct_ramp$kernel.class, "ramp")
  expect_equal(ct_ramp$type, "diverging")
  expect_length(ct_ramp$palette(3L), 3L)
  expect_equal(ct_ramp$ramp(c(0, 1)), c("#0000FF", "#FF0000"))

  # 4. NULL returns NULL; invalid object throws error
  expect_null(colortheme(NULL))
  expect_error(colortheme(12345), "can't be converted")
  expect_error(colortheme(character(0)), "single theme name or a vector")
})

test_that("colortheme() retrieves built-in themes and parses URL-like query syntax", {
  # Built-in themes from colortheme and grDevices
  ct_azurerose <- colortheme("azurerose")
  expect_equal(ct_azurerose$type, "diverging")
  expect_equal(ct_azurerose$name, "azurerose")
  expect_equal(ct_azurerose$source, "colortheme")

  ct_hcl <- colortheme("hcl")
  expect_equal(ct_hcl$type, "qualitative")
  expect_length(ct_hcl$palette(6L), 6L)

  # Syntax: source/name_r@type?query (supports both ';' and '&' delimiters)
  ct_syn <- colortheme("grDevices/Zissou 1_r@qual?alpha=0.5;na.color='#000000'")
  expect_equal(ct_syn$name, "Zissou 1")
  expect_equal(ct_syn$source, "grDevices")
  expect_equal(ct_syn$type, "qualitative")
  expect_equal(ct_syn$options$na.color, "#000000")
  expect_equal(ct_syn$kernel.args$alpha, 0.5)

  # Verify reversal via _r suffix
  ct_norm <- colortheme("bluescale")
  ct_rev  <- colortheme("bluescale_r")
  expect_equal(ct_norm$palette(5L), rev(ct_rev$palette(5L)))
  expect_equal(ct_norm$ramp(c(0.2, 0.8)), ct_rev$ramp(c(0.8, 0.2)))

  # Single valid color string ("red") is treated as a color vector if not registered
  ct_red <- colortheme("red")
  expect_equal(ct_red$kernel.class, "color")

  # Errors on unknown theme or malformed query
  expect_error(colortheme("non_existent_theme_xyz"), "not found")
  expect_error(colortheme("colortheme/non_existent"), "not found")
  expect_error(colortheme("azurerose?invalid_query_no_equals"), "can't be parsed")
})

test_that("programmable 'highlight' theme and stepped ramps (ramp.steps / 'steps') work", {
  # 1. 'highlight' with group list, negative index (-1 = last), and out-of-bounds safety
  ct_hl <- colortheme("highlight?which=list(1:2, -1, 99)")
  pal_hl <- ct_hl$palette(5L)
  expect_equal(pal_hl[1:2], c("#D64550", "#D64550"))
  expect_equal(pal_hl[3:4], c("#BFBFBF", "#BFBFBF"))
  expect_equal(pal_hl[5L], "#2B5C8F")
  expect_equal(ct_hl$palette(0L), character(0L))

  # Reversing 'highlight' reverses accent color order
  ct_hl_r <- colortheme("highlight_r?which=1")
  expect_equal(ct_hl_r$palette(2L), c("#2A9D8F", "#BFBFBF"))

  # 2. Stepped ramp via integer ramp.steps (k equal intervals)
  ct_step2 <- colortheme(c("#000000", "#FFFFFF"), options = list(ramp.steps = 2L))
  expect_equal(
    ct_step2$ramp(c(0.1, 0.4, 0.6, 0.9)),
    c("#000000", "#000000", "#FFFFFF", "#FFFFFF")
  )

  # 3. Stepped ramp via custom cutoffs in (0, 1)
  ct_cut <- colortheme(
    c("#0000FF", "#888888", "#FF0000"),
    options = list(ramp.steps = c(0.25, 0.75))
  )
  expect_equal(
    ct_cut$ramp(c(0.1, 0.5, 0.9)),
    c("#0000FF", "#888888", "#FF0000")
  )

  # 4. Built-in stepped themes ('steps' and 'bluesteps')
  ct_steps <- colortheme("steps")
  expect_equal(ct_steps$type, "diverging")
  expect_equal(ct_steps$options$ramp.steps, 11L)

  ct_bsteps <- colortheme("bluesteps")
  expect_equal(ct_bsteps$type, "sequential")
  expect_equal(ct_bsteps$options$ramp.steps, 11L)
})

test_that("colortheme modification, subset assignment, and formatting options work", {
  ct <- colortheme(c("#000000", "#FFFFFF"), type = "qualitative")
  expect_equal(ct$options$palette.formatter, "recycle")
  # Recycle when n > kernel.size (2)
  expect_equal(ct$palette(4L), c("#000000", "#FFFFFF", "#000000", "#FFFFFF"))

  # Qualitative theme defaults to stepped ramp
  expect_equal(ct$ramp(c(0.1, 0.9)), c("#000000", "#FFFFFF"))

  # Switching type updates default mode and formatter automatically
  ct_seq <- colortheme(ct, type = "sequential")
  expect_equal(ct_seq$type, "sequential")
  expect_equal(ct_seq$options$palette.formatter, "interpolate")
  expect_equal(ct_seq$kernel.args$mode, "ramp")

  # Formatter: fill.na and error
  ct_na <- colortheme(ct, options = list(palette.formatter = "fill.na", na.color = "gray"))
  expect_equal(ct_na$palette(3L), c("#000000", "#FFFFFF", "gray"))
  ct_err <- colortheme(ct, options = list(palette.formatter = "none"))
  expect_error(ct_err$palette(3L), "kernel.size")

  # Out-of-bounds and NA handling in ramp()
  expect_equal(
    ct_na$ramp(c(-0.5, NA, 1.5)),
    c("gray", "gray", "gray")
  )
  expect_equal(
    ct_na$ramp(c(-0.5, 0, 1, 1.5)),
    c("gray", "#000000", "#FFFFFF", "gray")
  )

  # Dynamic recompilation via $<- and [[<-
  ct_mod <- colortheme(c("#000000", "#FFFFFF"), type = "qualitative")
  ct_mod$options$na.color <- "#FF0000"
  expect_equal(ct_mod$ramp(NA_real_), "#FF0000")
  ct_mod[["type"]] <- "sequential"
  expect_equal(ct_mod$type, "sequential")

  # Identity when called with 1 argument
  expect_identical(colortheme(ct_mod), ct_mod)
})

test_that("print.colortheme and plot.colortheme execute cleanly", {
  ct_seq  <- colortheme("bluescale")
  ct_qual <- colortheme("hcl")

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_output(
    res <- print(ct_seq, display = TRUE),
    "Sequential Color Theme: 'bluescale' \\(from 'colortheme'\\)"
  )
  expect_output(print(ct_seq, display = FALSE), "Kernel Colors:")
  expect_identical(res, ct_seq)
  expect_output(
    print(ct_qual, display = FALSE),
    "Qualitative Color Theme: 'hcl' \\(from 'colortheme'\\)"
  )
  expect_output(print(ct_qual, display = FALSE), "Kernel Function:")
  expect_invisible(plot(ct_qual, n = 6L))
})

test_that("register_colortheme(), colorthemes(), and colortheme_options() work", {
  old_opts <- options()
  on.exit(options(old_opts), add = TRUE)

  tmp_env <- new.env(parent = emptyenv())

  # Empty registry returns 0-row data.frame with exact schema
  df_empty <- colorthemes(env = tmp_env)
  expect_s3_class(df_empty, "data.frame")
  expect_equal(nrow(df_empty), 0L)
  expect_equal(names(df_empty), c("name", "source", "type"))

  # Register custom theme from vector and from colortheme object
  old <- register_colortheme(
    kernel = c("blue", "white", "red"),
    name = "bwr", source = "test_src", type = "diverging", env = tmp_env
  )
  expect_null(old)

  ct_obj <- colortheme("bwr", env = tmp_env)
  old2 <- register_colortheme(ct_obj, name = "bwr", source = "test_src", env = tmp_env)
  expect_type(old2, "list")
  expect_equal(old2$name, "bwr")

  # Query registry with prefix and type filters
  df_all <- colorthemes(env = tmp_env)
  expect_equal(nrow(df_all), 1L)
  expect_equal(nrow(colorthemes(env = tmp_env, name = "BW", source = "test", type = "div")), 1L)
  expect_equal(nrow(colorthemes(env = tmp_env, name = "unknown")), 0L)

  # Remove registered theme by passing NULL
  removed <- register_colortheme(NULL, name = "bwr", source = "test_src", env = tmp_env)
  expect_equal(removed$name, "bwr")
  expect_equal(nrow(colorthemes(env = tmp_env)), 0L)

  # Validation errors in register_colortheme
  expect_error(register_colortheme(c("red", "blue"), name = "bad/name", env = tmp_env), "reserved")
  expect_error(register_colortheme(c("red", "blue"), name = "bad_r", env = tmp_env), "reserved")
  expect_error(register_colortheme(c("red", "blue"), name = "bad@q", env = tmp_env), "reserved")
  expect_error(register_colortheme(c("red", "blue"), name = "bad?a=1", env = tmp_env), "reserved")

  # Initialize full suite into temporary environment and clear
  init_colortheme(env = tmp_env, clear.env = TRUE)
  expect_gt(nrow(colorthemes(env = tmp_env)), 10L)

  # colortheme_options() get, set, restore via list, and warn on typos
  prev <- colortheme_options(sequential = "Viridis")
  expect_equal(colortheme_options("sequential")$sequential, "Viridis")
  colortheme_options(prev)
  expect_equal(colortheme_options("sequential")$sequential, prev$sequential)
  expect_warning(colortheme_options(unknown_key = 123), "typo|unrecognized")
})

test_that("map_to_colors() maps discrete and continuous values accurately", {
  # Continuous sequential
  x_num <- c(0, 5, 10, NA)
  c_seq <- map_to_colors(x_num, theme = c("#000000", "#FFFFFF"), na.value = "#FF0000")
  expect_equal(c_seq, c("#000000", "#777777", "#FFFFFF", "#FF0000"))

  # Continuous diverging with middle = 0
  x_div <- c(-10, 0, 5)
  ct_div <- colortheme(c("#0000FF", "#FFFFFF", "#FF0000"), type = "diverging")
  c_div <- map_to_colors(x_div, theme = ct_div, middle = 0)
  expect_equal(c_div[1L], "#0000FF")
  expect_equal(c_div[2L], "#FFFFFF")

  # Constant vector edge case (d == 0)
  expect_length(map_to_colors(c(5, 5, 5), theme = "bluescale"), 3L)
  expect_length(map_to_colors(c(0, 0, 0), theme = "azurerose", middle = 0), 3L)

  # Discrete mapping (character, factor, logical)
  x_chr <- c("A", "B", "A", NA)
  c_qual <- map_to_colors(
    x_chr,
    theme = colortheme(c("#111111", "#222222"), type = "qualitative"),
    na.value = "#999999"
  )
  expect_equal(c_qual, c("#111111", "#222222", "#111111", "#999999"))

  # Qualitative theme on factor / continuous variable
  c_disc <- map_to_colors(factor(1:5), theme = "hcl")
  expect_equal(c_disc, c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))
  c_q_cont <- map_to_colors(c(0, 1), theme = colortheme(c("#111111", "#222222"), type = "qualitative"))
  expect_equal(c_q_cont, c("#111111", "#222222"))
})

test_that("scale_color_theme() and scale_fill_theme() construct ggplot2 scales", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  s_disc <- scale_color_theme("hcl")
  expect_s3_class(s_disc, "ScaleDiscrete")

  s_seq <- scale_colour_theme("bluescale")
  expect_s3_class(s_seq, "ScaleContinuous")

  s_div <- scale_fill_theme("azurerose", middle = 0.5)
  expect_s3_class(s_div, "ScaleContinuous")
  expect_equal(s_div$aesthetics, "fill")

  # Force sequential theme into discrete scale
  s_forced <- scale_color_theme("bluescale", discrete = TRUE)
  expect_s3_class(s_forced, "ScaleDiscrete")

  # Force qualitative theme onto continuous scale (stepped ramp, no error)
  s_q_cont <- scale_color_theme("hcl", discrete = FALSE)
  expect_s3_class(s_q_cont, "ScaleContinuous")
})

test_that("as_palette() and as_ramp() extract color-generating functions", {
  # From theme name
  pal <- as_palette("Okabe-Ito")
  expect_type(pal, "closure")
  expect_length(pal(5L), 5L)

  ramp <- as_ramp("bluescale")
  expect_type(ramp, "closure")
  expect_length(ramp(c(0, 0.5, 1)), 3L)

  # From colortheme object with on-the-fly modification via ...
  ct <- colortheme("bluescale")
  pal_rev <- as_palette(ct, reverse = TRUE)
  expect_equal(pal_rev(3L), rev(ct$palette(3L)))

  ramp_rev <- as_ramp(ct, reverse = TRUE)
  expect_equal(ramp_rev(c(0, 1)), ct$ramp(c(1, 0)))

  # Direct color vector fast-path in as_ramp.default
  fast_ramp <- as_ramp(c("#000000", "#FFFFFF"))
  expect_s3_class(fast_ramp, "ramp")
  expect_equal(fast_ramp(c(0, 1)), c("#000000", "#FFFFFF"))
})
