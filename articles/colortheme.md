# Getting Started with Color Themes

## Color themes

The “color.theme” object provides two color-generating functions:
[`palette()`](https://rdrr.io/r/grDevices/palette.html) and `ramp()`.
The [`palette()`](https://rdrr.io/r/grDevices/palette.html) function
accepts an integer $n$ and returns a vector of $n$ discrete colors. It
is primarily intended for **qualitative** themes, where distinct colors
are used to represent categorical data. The `ramp()` function accepts a
numeric vector $x$ with values in the $\lbrack 0,1\rbrack$ interval and
returns a vector of corresponding colors. It maps numeric values onto a
continuous color gradient, making it suitable for **sequential** and
**diverging** themes.

### Pre-defined themes

You can get a pre-defined “color.theme” object by providing a theme name
to the
[`color.theme()`](https://ryo-asashi.github.io/midr/reference/color.theme.md)
function.

``` r
library(midr)
library(ggplot2)
library(gridExtra)
# diverging color theme "nightfall" (package:khroma)
nightfall <- color.theme("nightfall")
print(nightfall)
#> Diverging Color Theme : "nightfall"
```

![](colortheme_files/figure-html/unnamed-chunk-2-1.png)

``` r
nightfall$palette(5)
#> [1] "#125A56" "#60BCE9" "#ECEADA" "#FD9A44" "#A01813"
#> attr(,"missing")
#> [1] "#FFFFFF"
```

``` r
nightfall$ramp(c(0.00, 0.25, 0.50, 0.75, 1.00))
#> [1] "#125955" "#5FBBE9" "#EBEAD9" "#FD9944" "#9F1813"
```

``` r
# sequential color theme "viridis" (package:viridisLite)
viridis <- color.theme("viridis")
print(viridis)
#> Sequential Color Theme : "viridis"
```

![](colortheme_files/figure-html/unnamed-chunk-4-1.png)

``` r
nightfall$ramp(c(0.00, 0.25, 0.50, 0.75, 1.00))
#> [1] "#125955" "#5FBBE9" "#EBEAD9" "#FD9944" "#9F1813"
```

``` r
viridis$ramp(c(0.00, 0.25, 0.50, 0.75, 1.00))
#> [1] "#46337F" "#31658D" "#21908C" "#38B976" "#99D83B"
```

You can modify themes by **reversing** the color order or **changing**
the theme type (e.g., from sequential to qualitative). These changes can
be applied in two ways:

1.  **Using Arguments** : provide the appropriate argument to the
    function, such as `reverse = TRUE` or `type = "qualitative"`.
2.  **Using Suffixes** : for convenience, you can append a suffix
    directly to the theme’s name. `_r` to reverse the theme, `@q` (or
    longer, such as `@qual`) to make the theme qualitative (`@d` for
    diverging, `@s` for sequential).

``` r
plot(color.theme("nightfall", reverse = TRUE),
     text = "khroma/nightfall_r")
```

![](colortheme_files/figure-html/unnamed-chunk-6-1.png)

``` r
plot(color.theme("nightfall", type = "qualitative"),
     text = "khroma/nightfall@qual")
```

![](colortheme_files/figure-html/unnamed-chunk-6-2.png)

``` r
plot(color.theme("viridis_r",),
     text = "viridisLite/viridis_r")
```

![](colortheme_files/figure-html/unnamed-chunk-6-3.png)

``` r
plot(color.theme("viridis@qual"),
     text = "viridisLite/viridis@qual")
```

![](colortheme_files/figure-html/unnamed-chunk-6-4.png)

When multiple packages provide a theme with the same name (e.g.,
“Paired”), you must specify which one to use. You can do this in two
ways:

1.  **Using Argument** : provide the package name to the `source`
    argument (e.g., `source = "grDevices"`).
2.  **Using Prefix** : append a prefix to the theme name with the
    package name and a forward slash (e.g., `"RColorBrewer/Paired"`).

``` r
# qualitative color theme "Paired" (package:grDevices)
paired <- color.theme("Paired", source = "grDevices")
plot(paired, text = "grDevices/Paired")
```

![](colortheme_files/figure-html/unnamed-chunk-7-1.png)

``` r
# qualitative color theme "Paired" (package:RColorBrewer)
paired2 <- color.theme("RColorBrewer/Paired")
plot(paired2, text = "RColorBrewer/Paired")
```

![](colortheme_files/figure-html/unnamed-chunk-7-2.png)

### Custom themes

Alternatively, you can create a new “color.theme” object by passing a
custom color vector or function to the first argument of
[`color.theme()`](https://ryo-asashi.github.io/midr/reference/color.theme.md).

``` r
# create new color theme using a color vector
mytheme <- color.theme(
  c("#003f5c", "#7a5195", "#ef5675", "#ffa600"),
  type = "sequential", name = "mytheme"
)
print(mytheme)
#> Sequential Color Theme : "mytheme"
```

![](colortheme_files/figure-html/unnamed-chunk-8-1.png)

``` r
mytheme$palette(5)
#> [1] "#003F5B" "#614D86" "#B85485" "#F46C63" "#FFA500"
```

``` r
mytheme$ramp(c(0.00, 0.25, 0.50, 0.75, 1.00))
#> [1] "#003F5B" "#614D86" "#B85485" "#F46C63" "#FFA500"
```

``` r
# create new color theme using a color function
rainbow <- color.theme(grDevices::rainbow,
                       name = "rainbow", source = "grDevices")
print(rainbow)
#> Sequential Color Theme : "rainbow"
```

![](colortheme_files/figure-html/unnamed-chunk-10-1.png)

``` r
rainbow$palette(5)
#> [1] "#FF0000" "#CCFF00" "#00FF66" "#0066FF" "#CC00FF"
```

``` r
rainbow$ramp(c(0.00, 0.25, 0.50, 0.75, 1.00))
#> [1] "#FF0000" "#81FF00" "#00FFFB" "#7B00FF" "#FF0006"
```

You can register a custom theme to call it by name later in you current
R session. To do so, use the
[`set.color.theme()`](https://ryo-asashi.github.io/midr/reference/set.color.theme.md)
function.

``` r
set.color.theme(mytheme, name = "mytheme", source = "custom")
color.theme("mytheme_r@div")
#> Diverging Color Theme : "mytheme"
```

![](colortheme_files/figure-html/unnamed-chunk-12-1.png)

``` r
color.theme("custom/mytheme@q")
#> Qualitative Color Theme : "mytheme"
```

![](colortheme_files/figure-html/unnamed-chunk-12-2.png)

### Using themes in **midr**

The color appearance of visualizations created with **midr** can be
easily customized by passing a “color.theme” object or a pre-defined
color theme name (see below) to
[`ggmid()`](https://ryo-asashi.github.io/midr/reference/ggmid.md) or
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

``` r
set.seed(42)
dataset <- diamonds[sample(nrow(diamonds), 5000L), ]
mid <- interpret(price ~ (carat + color + clarity + cut) ^ 2, dataset)
#> 'model' not passed: response variable in 'data' is used
grid.arrange(
  ggmid(mid, "color:clarity", main.effect = TRUE),
  ggmid(mid, "color:clarity", main.effect = TRUE, theme = mytheme),
  ggmid(mid, "carat:color", main.effect = TRUE, theme = "tokyo_r"),
  ggmid(mid, "carat:color", main.effect = TRUE, theme = "bicolor")
)
```

![](colortheme_files/figure-html/unnamed-chunk-13-1.png)

``` r
imp <- mid.importance(mid)
grid.arrange(
  ggmid(imp, "heatmap"),
  ggmid(imp, "barplot", max = 10, theme = "mytheme@q"),
  ggmid(imp, "heatmap", theme = "mytheme_r"),
  ggmid(imp, "barplot", max = 10, theme = "highlight_r")
)
```

![](colortheme_files/figure-html/unnamed-chunk-13-2.png)

### Using themes with **ggplot2**

To apply your color themes to `ggplot2` plots, use the
[`scale_color_theme()`](https://ryo-asashi.github.io/midr/reference/scale_color_theme.md)
and
[`scale_fill_theme()`](https://ryo-asashi.github.io/midr/reference/scale_color_theme.md)
functions. These scales integrate your themes directly into the plot’s
`color` and `fill` aesthetics.

``` r
p <- ggplot(dataset) + geom_point(aes(carat, price, col = color))
grid.arrange(
  p + scale_color_theme("discreterainbow"),
  p + scale_color_theme("viridisLite/mako", discrete = TRUE),
  p + scale_color_theme("tokyo@qual"),
  p + scale_color_theme("highlight?base='#50505010'&which=6:7")
)
```

![](colortheme_files/figure-html/unnamed-chunk-14-1.png)

``` r
p <- ggplot(dataset) +
  geom_histogram(aes(x = carat, fill = cut), bins = 20)
grid.arrange(
  p + scale_fill_theme("muted_r"),
  p + scale_fill_theme("khroma/discreterainbow"),
  p + scale_fill_theme("mytheme@q"),
  p + scale_fill_theme("highlight?which=1:3&accent='#0da1d0'")
)
```

![](colortheme_files/figure-html/unnamed-chunk-15-1.png)

## Pre-defined color themes

The following color themes are available when the **midr** package is
loaded. Some themes depend on other packages being installed, so for
full functionality, please ensure you have already installed the
**viridisLite**, **RColorBrewer**, and **khroma** packages.

### Diverging Color Themes

![](colortheme_files/figure-html/unnamed-chunk-17-1.png)![](colortheme_files/figure-html/unnamed-chunk-17-2.png)![](colortheme_files/figure-html/unnamed-chunk-17-3.png)![](colortheme_files/figure-html/unnamed-chunk-17-4.png)![](colortheme_files/figure-html/unnamed-chunk-17-5.png)![](colortheme_files/figure-html/unnamed-chunk-17-6.png)![](colortheme_files/figure-html/unnamed-chunk-17-7.png)![](colortheme_files/figure-html/unnamed-chunk-17-8.png)![](colortheme_files/figure-html/unnamed-chunk-17-9.png)![](colortheme_files/figure-html/unnamed-chunk-17-10.png)![](colortheme_files/figure-html/unnamed-chunk-17-11.png)![](colortheme_files/figure-html/unnamed-chunk-17-12.png)![](colortheme_files/figure-html/unnamed-chunk-17-13.png)![](colortheme_files/figure-html/unnamed-chunk-17-14.png)![](colortheme_files/figure-html/unnamed-chunk-17-15.png)![](colortheme_files/figure-html/unnamed-chunk-17-16.png)

### Qualitative Color Themes

![](colortheme_files/figure-html/unnamed-chunk-18-1.png)![](colortheme_files/figure-html/unnamed-chunk-18-2.png)![](colortheme_files/figure-html/unnamed-chunk-18-3.png)![](colortheme_files/figure-html/unnamed-chunk-18-4.png)![](colortheme_files/figure-html/unnamed-chunk-18-5.png)![](colortheme_files/figure-html/unnamed-chunk-18-6.png)![](colortheme_files/figure-html/unnamed-chunk-18-7.png)![](colortheme_files/figure-html/unnamed-chunk-18-8.png)![](colortheme_files/figure-html/unnamed-chunk-18-9.png)![](colortheme_files/figure-html/unnamed-chunk-18-10.png)![](colortheme_files/figure-html/unnamed-chunk-18-11.png)![](colortheme_files/figure-html/unnamed-chunk-18-12.png)

### Sequential Color Themes

![](colortheme_files/figure-html/unnamed-chunk-19-1.png)![](colortheme_files/figure-html/unnamed-chunk-19-2.png)![](colortheme_files/figure-html/unnamed-chunk-19-3.png)![](colortheme_files/figure-html/unnamed-chunk-19-4.png)![](colortheme_files/figure-html/unnamed-chunk-19-5.png)![](colortheme_files/figure-html/unnamed-chunk-19-6.png)![](colortheme_files/figure-html/unnamed-chunk-19-7.png)![](colortheme_files/figure-html/unnamed-chunk-19-8.png)![](colortheme_files/figure-html/unnamed-chunk-19-9.png)![](colortheme_files/figure-html/unnamed-chunk-19-10.png)![](colortheme_files/figure-html/unnamed-chunk-19-11.png)![](colortheme_files/figure-html/unnamed-chunk-19-12.png)![](colortheme_files/figure-html/unnamed-chunk-19-13.png)![](colortheme_files/figure-html/unnamed-chunk-19-14.png)![](colortheme_files/figure-html/unnamed-chunk-19-15.png)![](colortheme_files/figure-html/unnamed-chunk-19-16.png)![](colortheme_files/figure-html/unnamed-chunk-19-17.png)![](colortheme_files/figure-html/unnamed-chunk-19-18.png)![](colortheme_files/figure-html/unnamed-chunk-19-19.png)![](colortheme_files/figure-html/unnamed-chunk-19-20.png)![](colortheme_files/figure-html/unnamed-chunk-19-21.png)![](colortheme_files/figure-html/unnamed-chunk-19-22.png)![](colortheme_files/figure-html/unnamed-chunk-19-23.png)![](colortheme_files/figure-html/unnamed-chunk-19-24.png)![](colortheme_files/figure-html/unnamed-chunk-19-25.png)![](colortheme_files/figure-html/unnamed-chunk-19-26.png)![](colortheme_files/figure-html/unnamed-chunk-19-27.png)![](colortheme_files/figure-html/unnamed-chunk-19-28.png)![](colortheme_files/figure-html/unnamed-chunk-19-29.png)![](colortheme_files/figure-html/unnamed-chunk-19-30.png)![](colortheme_files/figure-html/unnamed-chunk-19-31.png)![](colortheme_files/figure-html/unnamed-chunk-19-32.png)![](colortheme_files/figure-html/unnamed-chunk-19-33.png)
