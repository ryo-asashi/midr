library(midr)
library(ggplot2)
library(gridExtra)
theme_set(theme_midr())
data(CO2, package = "datasets")
mid <- interpret(uptake ~ (conc + Type + Treatment)^2,CO2,
                 type = 0, k = 0)
summary(mid)
grid.arrange(grobs = mid.plots(mid), nrow = 2)
mc <- mid.conditional(mid, "conc", CO2)
ggmid(mc,
      variable.colour = "Type",
      variable.linetype = Treatment) +
  ggtitle("ICE Curves")
ggmid(mid, "Treatment:conc", include = TRUE)
ggmid(mid, "Type:conc", include = TRUE)
