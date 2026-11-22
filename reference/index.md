# Package index

## Fit MID Models

- [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  : Fit MID Models

### Methods

- [`plot(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  [`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md) :
  Plot MID Component Functions
- [`plot(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mids.md)
  [`ggmid(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mids.md)
  : Compare MID Component Functions Across Models
- [`persp(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/persp.mid.md)
  : Perspective Plots of MID Component Functions
- [`predict(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)
  [`predict(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/predict.mid.md)
  : Predict Method for Fitted MID Models
- [`print(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/print.mid.md)
  [`print(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/print.mid.md)
  : Print MID Models
- [`summary(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/summary.mid.md)
  [`summary(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/summary.mid.md)
  : Summarize MID Models
- [`subset(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/subset.mid.md)
  [`subset(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/subset.mid.md)
  [`coef(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/subset.mid.md)
  [`coef(`*`<midrib>`*`)`](https://ryo-asashi.github.io/midr/reference/subset.mid.md)
  : Extract Submodels and Coefficients from MID Models

## Combine MID Objects

- [`midlist()`](https://ryo-asashi.github.io/midr/reference/midlist.md)
  [`as.midlist()`](https://ryo-asashi.github.io/midr/reference/midlist.md)
  : Combine MID Objects

### Methods

- [`` `[`( ``*`<midlist>`*`)`](https://ryo-asashi.github.io/midr/reference/extract.midlist.md)
  [`` `[`( ``*`<midrib>`*`)`](https://ryo-asashi.github.io/midr/reference/extract.midlist.md)
  [`` `[[`( ``*`<midrib>`*`)`](https://ryo-asashi.github.io/midr/reference/extract.midlist.md)
  : Subset MID Collection Objects
- [`labels(`*`<midlist>`*`)`](https://ryo-asashi.github.io/midr/reference/labels.midlist.md)
  [`labels(`*`<midrib>`*`)`](https://ryo-asashi.github.io/midr/reference/labels.midlist.md)
  [`` `labels<-`() ``](https://ryo-asashi.github.io/midr/reference/labels.midlist.md)
  : Label MID Collection Objects

## Term or Variable Importance

- [`eval_importance()`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
  : Evaluate Term or Variable Importance

### Methods

- [`plot(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  [`ggmid(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  : Plot Term or Variable Importance
- [`plot(`*`<midimps>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimps.md)
  [`ggmid(`*`<midimps>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimps.md)
  : Compare Term or Variable Importance Across Models

## Prediction Breakdowns

- [`eval_breakdown()`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
  : Evaluate Prediction Breakdowns

### Methods

- [`plot(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  [`ggmid(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  : Plot Prediction Breakdown
- [`plot(`*`<midbrks>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrks.md)
  [`ggmid(`*`<midbrks>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrks.md)
  : Compare Prediction Breakdowns Across Models

## Conditional Expectations

- [`eval_conditional()`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
  : Evaluate Conditional Expectations

### Methods

- [`plot(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  [`ggmid(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  : Plot Conditional Expectations
- [`plot(`*`<midcons>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcons.md)
  [`ggmid(`*`<midcons>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcons.md)
  : Compare Conditional Expectations Across Models

## Other Functions

- [`get_yhat()`](https://ryo-asashi.github.io/midr/reference/get_yhat.md)
  : Wrapper Prediction Function
- [`make_link()`](https://ryo-asashi.github.io/midr/reference/make_link.md)
  : Extended Parametric Link Functions
- [`make_encoder()`](https://ryo-asashi.github.io/midr/reference/make_encoder.md)
  [`encode()`](https://ryo-asashi.github.io/midr/reference/make_encoder.md)
  : Create Variable Encoders and Encode Data

### Miscellaneous

- [`midr_options()`](https://ryo-asashi.github.io/midr/reference/midr_options.md)
  : Set or Get Global Options for midr
- [`theme_monograph()`](https://ryo-asashi.github.io/midr/reference/theme_monograph.md)
  : Monograph Visual Theme for ggplot2
- [`par_monograph()`](https://ryo-asashi.github.io/midr/reference/par_monograph.md)
  : Monograph Graphical Parameters for Base Graphics
