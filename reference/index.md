# Package index

## Fit MID models

- [`interpret()`](https://ryo-asashi.github.io/midr/reference/interpret.md)
  : Fit MID Models

### Methods

- [`plot(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  [`ggmid()`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  [`autoplot(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  [`plot(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  [`autoplot(`*`<mids>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.mid.md)
  : Plot and Compare MID Component Functions
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
  : Extract Submodels from MID Models
- [`coef(`*`<mid>`*`)`](https://ryo-asashi.github.io/midr/reference/coef.mid.md)
  : Extract Component Effects from MID Models

## Combine MID objects

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

## Importance of effects

- [`eval_importance()`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
  [`print(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
  [`print(`*`<midimps>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
  [`summary(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
  [`summary(`*`<midimps>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
  [`as.data.frame(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_importance.md)
  : Evaluate Importance of Model Components

### Methods

- [`plot(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  [`ggmid(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  [`autoplot(`*`<midimp>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  [`plot(`*`<midimps>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  [`ggmid(`*`<midimps>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  [`autoplot(`*`<midimps>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midimp.md)
  : Plot and Compare MID Importance

## Breakdown of predictions

- [`eval_breakdown()`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
  [`print(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
  [`print(`*`<midbrks>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
  [`summary(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
  [`summary(`*`<midbrks>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
  [`as.data.frame(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_breakdown.md)
  : Evaluate Breakdown of Model Predictions

### Methods

- [`plot(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  [`ggmid(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  [`autoplot(`*`<midbrk>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  [`plot(`*`<midbrks>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  [`ggmid(`*`<midbrks>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  [`autoplot(`*`<midbrks>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midbrk.md)
  : Plot and Compare MID Breakdown

## Conditional expectations

- [`eval_conditional()`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
  [`print(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
  [`print(`*`<midcons>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
  [`summary(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
  [`summary(`*`<midcons>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
  [`as.data.frame(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/eval_conditional.md)
  : Evaluate Individual Conditional Expectation (ICE)

### Methods

- [`plot(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  [`ggmid(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  [`autoplot(`*`<midcon>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  [`plot(`*`<midcons>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  [`ggmid(`*`<midcons>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  [`autoplot(`*`<midcons>`*`)`](https://ryo-asashi.github.io/midr/reference/plot.midcon.md)
  : Plot and Compare MID Conditional Expectations (ICE)

## Other functions

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
