# chartCorrelation
Added features of displaying p-value and regression line to chart.Correlation (from R function).

p-value
```
text(0.5, 0.1, sprintf("%g",test$p.value))
```


regression
```
panel.regression <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                              cex = 1, col.regres = "red", ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    abline(stats::lm(y[ok] ~ x[ok]), col = col.regres, ...)
}
```

You can use this function as below
chart.Correlation2(df, method = "pearson or kendall or spearman")
Default method is "pearson". (same as chart.Correlation)
