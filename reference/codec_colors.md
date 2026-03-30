# CoDEC colors

CoDEC colors

## Usage

``` r
codec_colors(n = NULL)
```

## Arguments

- n:

  a numeric vector of color numbers or character vector of color names;
  if NULL returns named vector of available colors

## Examples

``` r
plot(1:8, rep(1, 8), col = codec_colors(1:8),
  pch = 19, cex = 10, axes = FALSE, xlab = "", ylab = "")
text(1:8, rep(1.1, 8), labels = names(codec_colors()))
```
