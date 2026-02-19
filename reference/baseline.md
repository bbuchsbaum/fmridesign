# Create a Baseline Specification

Generates a baselinespec for modeling low-frequency drift in fMRI time
series.

## Usage

``` r
baseline(
  degree = 1,
  basis = c("constant", "poly", "bs", "ns"),
  name = NULL,
  intercept = c("runwise", "global", "none")
)
```

## Arguments

- degree:

  Number of basis terms per image block (ignored for "constant").

- basis:

  Type of basis ("constant", "poly", "bs", or "ns").

- name:

  Optional name for the term.

- intercept:

  Type of intercept to include ("runwise", "global", or "none").

## Value

A baselinespec list instance.

## Examples

``` r
baseline(degree = 3, basis = "bs")
#> $degree
#> [1] 3
#> 
#> $basis
#> [1] "bs"
#> 
#> $fun
#> function (x, df = NULL, knots = NULL, degree = 3, intercept = FALSE, 
#>     Boundary.knots = range(x), warn.outside = TRUE) 
#> {
#>     ord <- 1L + (degree <- as.integer(degree))
#>     if (ord <= 1) 
#>         stop("'degree' must be integer >= 1")
#>     nx <- names(x)
#>     x <- as.vector(x)
#>     nax <- is.na(x)
#>     if (nas <- any(nax)) 
#>         x <- x[!nax]
#>     outside <- if (!missing(Boundary.knots)) {
#>         Boundary.knots <- sort(Boundary.knots)
#>         (ol <- x < Boundary.knots[1L]) | (or <- x > Boundary.knots[2L])
#>     }
#>     else FALSE
#>     if (mk.knots <- !is.null(df) && is.null(knots)) {
#>         nIknots <- df - ord + (1L - intercept)
#>         if (nIknots < 0L) {
#>             nIknots <- 0L
#>             warning(gettextf("'df' was too small; have used %d", 
#>                 ord - (1L - intercept)), domain = NA)
#>         }
#>         knots <- if (nIknots > 0L) {
#>             knots <- seq.int(from = 0, to = 1, length.out = nIknots + 
#>                 2L)[-c(1L, nIknots + 2L)]
#>             quantile(x[!outside], knots, names = FALSE)
#>         }
#>     }
#>     else if (!all(is.finite(knots))) 
#>         stop("non-finite knots")
#>     if (mk.knots && length(knots) && any(lrEq <- range(knots) %in% 
#>         Boundary.knots)) {
#>         if (lrEq[1L]) {
#>             aE <- all(i <- knots == (piv <- Boundary.knots[1L]))
#>             if (aE) 
#>                 warning("all interior knots match left boundary knot")
#>             else knots[i] <- knots[i] + (min(knots[knots > piv]) - 
#>                 piv)/8
#>         }
#>         if (lrEq[2L]) {
#>             aE2 <- all(i <- knots == (piv <- Boundary.knots[2L]))
#>             if (aE2) 
#>                 warning("all interior knots match right boundary knot")
#>             else knots[i] <- knots[i] - (piv - max(knots[knots < 
#>                 piv]))/8
#>         }
#>         if (!(lrEq[1L] && aE || lrEq[2L] && aE2)) 
#>             warning("shoving 'interior' knots matching boundary knots to inside")
#>     }
#>     Aknots <- sort(c(rep(Boundary.knots, ord), knots))
#>     if (any(outside)) {
#>         if (warn.outside) 
#>             warning("some 'x' values beyond boundary knots may cause ill-conditioned bases")
#>         derivs <- 0:degree
#>         scalef <- gamma(1L:ord)
#>         basis <- array(0, c(length(x), length(Aknots) - degree - 
#>             1L))
#>         e <- 1/4
#>         if (any(ol)) {
#>             k.pivot <- (1 - e) * Boundary.knots[1L] + e * Aknots[ord + 
#>                 1]
#>             xl <- cbind(1, outer(x[ol] - k.pivot, 1L:degree, 
#>                 `^`))
#>             tt <- splineDesign(Aknots, rep(k.pivot, ord), ord, 
#>                 derivs)
#>             basis[ol, ] <- xl %*% (tt/scalef)
#>         }
#>         if (any(or)) {
#>             k.pivot <- (1 - e) * Boundary.knots[2L] + e * Aknots[length(Aknots) - 
#>                 ord]
#>             xr <- cbind(1, outer(x[or] - k.pivot, 1L:degree, 
#>                 `^`))
#>             tt <- splineDesign(Aknots, rep(k.pivot, ord), ord, 
#>                 derivs)
#>             basis[or, ] <- xr %*% (tt/scalef)
#>         }
#>         if (any(inside <- !outside)) 
#>             basis[inside, ] <- splineDesign(Aknots, x[inside], 
#>                 ord)
#>     }
#>     else basis <- splineDesign(Aknots, x, ord)
#>     if (!intercept) 
#>         basis <- basis[, -1L, drop = FALSE]
#>     n.col <- ncol(basis)
#>     if (nas) {
#>         nmat <- matrix(NA, length(nax), n.col)
#>         nmat[!nax, ] <- basis
#>         basis <- nmat
#>     }
#>     dimnames(basis) <- list(nx, 1L:n.col)
#>     a <- list(degree = degree, knots = if (is.null(knots)) numeric(0L) else knots, 
#>         Boundary.knots = Boundary.knots, intercept = intercept)
#>     attributes(basis) <- c(attributes(basis), a)
#>     class(basis) <- c("bs", "basis", "matrix")
#>     basis
#> }
#> <bytecode: 0x557bfdc41d80>
#> <environment: namespace:splines>
#> 
#> $intercept
#> [1] "runwise"
#> 
#> $name
#> [1] "baseline_bs_3"
#> 
#> attr(,"class")
#> [1] "baselinespec" "nuisancespec"
```
