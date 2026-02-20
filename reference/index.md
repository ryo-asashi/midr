# Package index

## Fit MID models

- [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  : Fit MID Models

### Methods

- [`plot(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  [`plot(`*`<midlist>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  : Plot MID Component Functions
- [`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
  [`autoplot(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.md)
  : Plot MID Component Functions with ggplot2
- [`mid.plots()`](https://ryo-asashi.github.io/midr/reference/mid.plots.md)
  : Plot Multiple MID Component Functions
- [`predict(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)
  [`predict(`*`<midlist>`*`)`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)
  : Predict Method for fitted MID Models
- [`mid.effect()`](https://ryo-asashi.github.io/midr/reference/mid.effect.md)
  [`mid.f()`](https://ryo-asashi.github.io/midr/reference/mid.effect.md)
  : Evaluate Single MID Component Functions
- [`print(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/print.mid.md)
  : Print MID Models
- [`summary(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/summary.mid.md)
  : Summarize MID Models
- [`mid.terms()`](https://ryo-asashi.github.io/midr/reference/mid.terms.md)
  : Extract Terms from MID Models

## Importance of effects

- [`mid.importance()`](https://ryo-asashi.github.io/midr/reference/mid.importance.md)
  : Calculate MID Importance

### Methods

- [`plot(`*`<mid.importance>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.importance.md)
  [`plot(`*`<midlist.importance>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.importance.md)
  : Plot MID Importance
- [`ggmid(`*`<mid.importance>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.importance.md)
  [`autoplot(`*`<mid.importance>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.importance.md)
  [`ggmid(`*`<midlist.importance>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.importance.md)
  : Plot MID Importance with ggplot2

## Breakdown of predictions

- [`mid.breakdown()`](https://ryo-asashi.github.io/midr/reference/mid.breakdown.md)
  : Calculate MID Breakdowns

### Methods

- [`plot(`*`<mid.breakdown>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.breakdown.md)
  [`plot(`*`<midlist.breakdown>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.breakdown.md)
  : Plot MID Breakdowns
- [`ggmid(`*`<mid.breakdown>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.breakdown.md)
  [`autoplot(`*`<mid.breakdown>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.breakdown.md)
  [`ggmid(`*`<midlist.breakdown>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.breakdown.md)
  : Plot MID Breakdowns with ggplot2

## Conditional expectations

- [`mid.conditional()`](https://ryo-asashi.github.io/midr/reference/mid.conditional.md)
  : Calculate MID Conditional Expectations

### Methods

- [`plot(`*`<mid.conditional>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.conditional.md)
  [`plot(`*`<midlist.conditional>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.conditional.md)
  : Plot MID Conditional Expectations
- [`ggmid(`*`<mid.conditional>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.conditional.md)
  [`autoplot(`*`<mid.conditional>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.conditional.md)
  [`ggmid(`*`<midlist.conditional>`*`)`](https://ryo-asashi.github.io/midr/reference/ggmid.mid.conditional.md)
  : Plot MID Conditional Expectations with ggplot2

## Other functions

- [`get.yhat()`](https://ryo-asashi.github.io/midr/reference/get.yhat.md)
  : Wrapper Prediction Function
- [`get.link()`](https://ryo-asashi.github.io/midr/reference/get.link.md)
  : Extended Parametric Link Functions

### Variable encoders

- [`factor.encoder()`](https://ryo-asashi.github.io/midr/reference/factor.encoder.md)
  [`factor.frame()`](https://ryo-asashi.github.io/midr/reference/factor.encoder.md)
  : Encoder for Qualitative Variables
- [`numeric.encoder()`](https://ryo-asashi.github.io/midr/reference/numeric.encoder.md)
  [`numeric.frame()`](https://ryo-asashi.github.io/midr/reference/numeric.encoder.md)
  : Encoder for Quantitative Variables

### Color themes

- [`color.theme()`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
  : Color Themes for Graphics
- [`color.theme.info()`](https://ryo-asashi.github.io/midr/reference/color.theme.info.md)
  [`color.theme.env()`](https://ryo-asashi.github.io/midr/reference/color.theme.info.md)
  : Retrieve Color Theme Information
- [`set.color.theme()`](https://ryo-asashi.github.io/midr/reference/set.color.theme.md)
  : Register Color Themes
- [`scale_color_theme()`](https://ryo-asashi.github.io/midr/reference/scale_color_theme.md)
  [`scale_colour_theme()`](https://ryo-asashi.github.io/midr/reference/scale_color_theme.md)
  [`scale_fill_theme()`](https://ryo-asashi.github.io/midr/reference/scale_color_theme.md)
  : Color Theme Scales for ggplot2 Graphics

### Miscellaneous

- [`theme_midr()`](https://ryo-asashi.github.io/midr/reference/theme_midr.md)
  [`par.midr()`](https://ryo-asashi.github.io/midr/reference/theme_midr.md)
  : Default Plotting Themes
- [`weighted.loss()`](https://ryo-asashi.github.io/midr/reference/weighted.loss.md)
  : Weighted Loss Function
- [`shapviz(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/shapviz.mid.md)
  : Calculate MID-Derived Shapley Values
