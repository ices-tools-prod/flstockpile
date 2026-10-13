# Comparing the stockpile with SAG

The official results of an ICES stock assessment are the summary tables
in the ICES Stock Assessment Graphs database, [SAG](https://sg.ices.dk):
spawning stock biomass, fishing pressure, recruitment and catches by
year. The stockpile holds the detail behind those tables, the numbers,
weights, maturity and mortality by age. If a submitted stock object
really is the assessment that the advice was based on, the summary
calculated from it should be the same as the summary in SAG.

This article makes that comparison, for one stock and then for all of
them, using the [icesSAG](https://github.com/ices-tools-prod/icesSAG)
package to read SAG and the [FLCore](https://flr-project.org) package to
calculate the summary. It is run against the live SAG database each time
this site is built, so the results change as assessments are published
or corrected.

``` r

library(FLStockpile)
library(FLCore)
library(icesSAG)
library(ggplot2)

options(FLStockpile.url = "https://github.com/neilcampbelll/FLstockpile/releases/download/v2026.001")
```

## Summary of a stock

The summary of a stock in the stockpile is calculated from its `FLStock`
object. The spawning stock biomass, mean fishing mortality and
recruitment come from the model estimates. The catches in SAG are
usually the observed catches, so the catches come from the observed data
where the stockpile has them, and from the estimates otherwise.

``` r

# The summary of a stock assessment in the stockpile.
#
# Returns a data frame with one row per quantity and year.

stockpile_summary <- function(stock_code, assessment_year) {
  estimated <- stockpile_flstock(stock_code, assessment_year, type = "estimated")
  version <- stockpile_stocks(stock_code, assessment_year)
  catches <- if (version$observed) {
    catch(stockpile_flstock(stock_code, assessment_year, type = "observed"))
  } else {
    catch(estimated)
  }

  quantities <- list(SSB = ssb(estimated), F = fbar(estimated), recruitment = rec(estimated), catches = catches)
  rows <- lapply(names(quantities), function(quantity) {
    values <- as.data.frame(quantities[[quantity]])
    data.frame(quantity = quantity, year = values$year, stockpile = values$data)
  })
  do.call(rbind, rows)
}

# The summary of a stock assessment in SAG, in the same layout.

sag_summary <- function(stock_code, assessment_year) {
  sag <- getSAG(stock_code, assessment_year)
  rows <- lapply(c("SSB", "F", "recruitment", "catches"), function(quantity) {
    data.frame(quantity = quantity, year = sag$Year, sag = sag[[quantity]])
  })
  do.call(rbind, rows)
}

# The two summaries side by side, for the years and quantities in both.

compare_summaries <- function(stock_code, assessment_year) {
  both <- merge(stockpile_summary(stock_code, assessment_year), sag_summary(stock_code, assessment_year))
  both <- both[!is.na(both$stockpile) & !is.na(both$sag) & both$sag != 0, ]
  both$ratio <- both$stockpile / both$sag
  both[order(both$quantity, both$year), ]
}
```

## One stock

Sole in the eastern English Channel, `sol.27.7d`, has both observed data
and model estimates in the stockpile.

``` r

sole <- compare_summaries("sol.27.7d", 2026)
tail(sole[sole$quantity == "SSB", ], 5)
#>     quantity year stockpile   sag     ratio
#> 172      SSB 2021 11937.797 11938 0.9999830
#> 173      SSB 2022  9691.269  9691 1.0000278
#> 174      SSB 2023 10193.933 10194 0.9999934
#> 175      SSB 2024 10143.750 10144 0.9999753
#> 176      SSB 2025  8874.998  8875 0.9999998
tail(sole[sole$quantity == "catches", ], 5)
#>    quantity year stockpile  sag    ratio
#> 40  catches 2021  1914.027 1914 1.000014
#> 41  catches 2022  2012.486 2012 1.000242
#> 42  catches 2023  1499.205 1499 1.000137
#> 43  catches 2024  1574.047 1574 1.000030
#> 44  catches 2025  1251.492 1251 1.000393
```

``` r

ggplot(sole, aes(year)) +
  geom_point(aes(y = sag, colour = "SAG")) +
  geom_line(aes(y = stockpile, colour = "stockpile")) +
  facet_wrap(~quantity, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL, colour = NULL, title = "sol.27.7d, assessment year 2026") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![Four panels, one each for catches, F, recruitment and SSB of sol.27.7d
by year, with the SAG values as points and the stockpile values as a
line.](compare-with-sag_files/figure-html/unnamed-chunk-5-1.png)

The largest difference, in any year, for each quantity:

``` r

aggregate(ratio ~ quantity, sole, function(ratio) max(abs(ratio - 1)))
#>      quantity        ratio
#> 1     catches 3.932854e-04
#> 2           F 2.117354e-03
#> 3 recruitment 3.755920e-05
#> 4         SSB 3.979648e-05
```

## All stocks

The same comparison is made for the latest version of every stock
assessment in the stockpile that has model estimates. Stocks with more
than one season, unit or area are left out, because their summary cannot
be calculated in the same simple way, and so are stocks that are not
found in SAG.

``` r

stocks <- stockpile_stocks()
stocks <- stocks[stocks$estimated, ]
stocks$simple <- stocks$n_seasons == 1 & stocks$n_units == 1 & stocks$n_areas == 1

comparisons <- lapply(which(stocks$simple), function(i) {
  compared <- try(compare_summaries(stocks$stock_code[i], stocks$assessment_year[i]), silent = TRUE)
  if (inherits(compared, "try-error") || nrow(compared) == 0) {
    return(NULL)
  }
  cbind(stock_code = stocks$stock_code[i], assessment_year = stocks$assessment_year[i], compared)
})
comparisons <- do.call(rbind, comparisons)

stocks$compared <- paste(stocks$stock_code, stocks$assessment_year) %in%
  paste(comparisons$stock_code, comparisons$assessment_year)
```

Of the 43 stock assessments with estimates, 36 could be compared with
SAG. Not compared because they have seasons, units or areas:
pra.27.3a4a, san.sa.1r, san.sa.2r, san.sa.3r. Not found in SAG:
meg.27.7b-k8abd, nep.fu.32, whg.27.6a.

A stock and quantity is taken to be the same in the stockpile and in SAG
if they are within 2.5% of each other in every year. SAG holds rounded
values, so small differences are expected: a fishing mortality of 0.02
held to three decimal places can be 2.5% out.

``` r

agreement <- aggregate(
  ratio ~ stock_code + assessment_year + quantity, comparisons,
  function(ratio) max(abs(ratio - 1)) < 0.025
)
names(agreement)[4] <- "same"

table(agreement$quantity, ifelse(agreement$same, "same", "different"))
#>              
#>               different same
#>   catches            14   12
#>   F                   7   29
#>   recruitment         7   27
#>   SSB                 5   31
```

``` r

all_same <- aggregate(same ~ stock_code + assessment_year, agreement, all)
```

15 of the 36 stock assessments are the same as SAG in every quantity
that could be compared.

### Explore a stock

Choose a stock to see its summary in the stockpile and in SAG, as a plot
and as a table. The points are the SAG values and the lines the
stockpile values.

``` r

library(plotly)
library(crosstalk)
library(DT)

# one row per stock and year, with the stockpile and SAG value of each quantity
explore <- comparisons[c("year", "quantity", "stockpile", "sag")]
explore$stock <- paste(comparisons$stock_code, comparisons$assessment_year)
explore <- reshape(explore, idvar = c("stock", "year"), timevar = "quantity", direction = "wide")
explore <- explore[order(explore$stock, explore$year), ]

shared <- SharedData$new(explore, key = ~ paste(stock, year))

# the plot of one quantity
panel <- function(quantity) {
  plot_ly(shared, x = ~year) |>
    add_markers(
      y = as.formula(paste0("~sag.", quantity)), name = "SAG", legendgroup = "SAG",
      showlegend = quantity == "SSB", marker = list(color = "#d95f02")
    ) |>
    add_lines(
      y = as.formula(paste0("~stockpile.", quantity)), name = "stockpile", legendgroup = "stockpile",
      showlegend = quantity == "SSB", line = list(color = "#1b9e77")
    ) |>
    layout(
      yaxis = list(rangemode = "tozero", title = quantity),
      xaxis = list(title = "")
    )
}

quantities <- c("SSB", "F", "recruitment", "catches")
values <- c(rbind(paste0("stockpile.", quantities), paste0("sag.", quantities)))
```

Stock

``` r

subplot(lapply(quantities, panel), nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.06) |>
  layout(legend = list(orientation = "h", y = -0.1)) |>
  config(displayModeBar = FALSE)
```

``` r

datatable(
  shared,
  rownames = FALSE,
  style = "bootstrap5",
  class = "compact stripe",
  colnames = c("stock", "year", gsub(".", " ", values, fixed = TRUE)),
  options = list(
    columnDefs = list(list(targets = 0, visible = FALSE)),
    order = list(list(1, "desc")),
    pageLength = 10,
    dom = "tip"
  )
) |>
  formatSignif(values, digits = 4)
```

### Where they differ

For each stock and quantity that is not the same, the median over the
years of the stockpile value divided by the SAG value, and the range. A
blank means the quantity is the same, and `-` that it could not be
compared.

``` r

ratios <- aggregate(
  ratio ~ stock_code + assessment_year + quantity, comparisons,
  function(ratio) {
    if (max(abs(ratio - 1)) < 0.025) {
      ""
    } else {
      sprintf("%s (%s to %s)", signif(median(ratio), 3), signif(min(ratio), 3), signif(max(ratio), 3))
    }
  }
)
ratios <- reshape(ratios, idvar = c("stock_code", "assessment_year"), timevar = "quantity", direction = "wide")
names(ratios) <- sub("ratio.", "", names(ratios), fixed = TRUE)
ratios[is.na(ratios)] <- "-"
ratios <- ratios[rowSums(ratios[-(1:2)] != "" & ratios[-(1:2)] != "-") > 0, ]

knitr::kable(ratios[order(ratios$stock_code), ], row.names = FALSE)
```

| stock_code | assessment_year | catches | F | recruitment | SSB |
|:---|---:|:---|:---|:---|:---|
| ank.27.78abd | 2026 | 1.11 (0.926 to 1.31) |  | 1 (1 to 2.22) |  |
| ank.27.8c9a | 2026 | 1.01 (0.957 to 1.05) |  |  | 1.61 (1.43 to 1.76) |
| bll.27.3a47de | 2026 | \- | 0.336 (0.335 to 0.336) | \- | 6160 (5110 to 10100) |
| cod.27.1-2.coastN | 2026 | \- |  | 1.83 (1.3 to 2.84) |  |
| had.27.46a20 | 2026 | 0.991 (0.759 to 1.28) |  | 1 (0.518 to 1) |  |
| had.27.5a | 2026 |  | 1.34 (0.995 to 1.98) |  |  |
| had.27.6b | 2026 | 0.975 (0.608 to 1.29) |  |  |  |
| had.27.7b-k | 2026 | 0.997 (0.917 to 1) |  |  |  |
| her.27.20-24 | 2026 | 0.983 (0.853 to 1.07) | 1 (0.996 to 1.03) |  |  |
| her.27.5a | 2026 | 1 (0.965 to 1.74) | 1.2 (0.847 to 2.45) |  |  |
| hke.27.3a46-8abd | 2026 | 1.01 (0.96 to 1.09) |  |  |  |
| hke.27.8c9a | 2026 | 0.999 (0.909 to 1.01) |  |  |  |
| lez.27.4a6a | 2026 | 1 (0.961 to 1.07) | 0.169 (0.148 to 0.194) | \- | 36400 (36100 to 37000) |
| mon.27.8c9a | 2026 | 1.01 (0.985 to 1.06) |  |  |  |
| ple.27.21-32 | 2026 | \- | 0.995 (0.962 to 2.12) | 0.995 (0.937 to 1.7) | 0.996 (0.917 to 1.02) |
| pok.27.5a | 2026 |  | 1.14 (0.95 to 1.45) |  |  |
| sol.27.20-24 | 2026 | \- |  |  | 1 (0.892 to 1.11) |
| sol.27.8ab | 2026 | 1 (0.896 to 1.13) |  |  |  |
| spr.27.22-32 | 2024 | 0.992 (0.937 to 1.02) |  | 0.001 (0.000999 to 0.00101) |  |
| spr.27.22-32 | 2026 | 0.991 (0.937 to 1.02) |  | 0.001 (0.000997 to 0.001) |  |
| whg.27.47d | 2026 |  |  | 1 (0.861 to 1) |  |

There are several reasons for a difference, and most of them are not
errors:

- **Catches.** The stockpile catches are the estimates where no observed
  data were submitted, and SAG usually has the observed catches. A
  median close to 1 with a range around it is what that looks like.
- **Units.** SAG may hold a quantity in different units, for example
  recruitment in millions against thousands in the stock object, giving
  a ratio of 0.001 or 1000.
- **Relative quantities.** For some stocks SAG holds the biomass and
  fishing pressure relative to a reference point, such as B/B_(MSY), or
  a harvest rate in place of a fishing mortality. The description of
  each quantity is in the `stockSizeDescription` and
  `fishingPressureDescription` columns of
  [`getSAG()`](https://rdrr.io/pkg/icesSAG/man/getSAG.html).
- **Definitions.** The spawning stock biomass or recruitment published
  may be defined differently from the standard calculation used here,
  for example at another time of year or at another age.
- **A different assessment.** If none of these apply, the submitted
  stock object may not be the assessment that was published.

The stocks compared, with how SAG describes their quantities:

``` r

descriptions <- lapply(which(stocks$compared), function(i) {
  sag <- getSAG(stocks$stock_code[i], stocks$assessment_year[i])
  data.frame(
    stock_code = stocks$stock_code[i],
    assessment_year = stocks$assessment_year[i],
    stock_size = sag$stockSizeDescription[1],
    stock_size_units = sag$stockSizeUnits[1],
    fishing_pressure = sag$fishingPressureDescription[1],
    recruitment_age = sag$recruitment_age[1]
  )
})

knitr::kable(do.call(rbind, descriptions), row.names = FALSE)
```

| stock_code | assessment_year | stock_size | stock_size_units | fishing_pressure | recruitment_age |
|:---|---:|:---|:---|:---|---:|
| ank.27.78abd | 2026 | Combined-sex SSB | tonnes | NA | 0 |
| ank.27.8c9a | 2026 | SSB | tonnes | F | 0 |
| bll.27.3a47de | 2026 | B/Bmsy | NA | F/Fmsy | NA |
| caa.27.5a | 2026 | SSB | tonnes | F | 4 |
| cod.27.1-2.coastN | 2026 | SSB | tonnes | F | 3 |
| had.27.46a20 | 2026 | SSB | tonnes | F | 0 |
| had.27.5a | 2026 | SSB | NA | Harvest rate | 1 |
| had.27.6b | 2026 | SSB | tonnes | F | 1 |
| had.27.7b-k | 2026 | NA | tonnes | NA | 0 |
| her.27.20-24 | 2026 | SSB | tonnes | F | 0 |
| her.27.3031 | 2024 | SSB | tonnes | F | 0 |
| her.27.3031 | 2026 | SSB | tonnes | F | 0 |
| her.27.5a | 2026 | SSB | NA | HR | 2 |
| hke.27.3a46-8abd | 2026 | SSB | tonnes | F | 0 |
| hke.27.8c9a | 2026 | SSB | tonnes | F | 0 |
| hom.27.9a | 2026 | SSB | tonnes | F | 0 |
| lez.27.4a6a | 2026 | B/Bmsy | tonnes | F/Fmsy | NA |
| lin.27.5a | 2026 | SSB | NA | F | 2 |
| mon.27.8c9a | 2026 | SSB | tonnes | F | 0 |
| ple.27.21-32 | 2026 | SSB | tonnes | F | 1 |
| ple.27.5a | 2026 | SSB | NA | F | 3 |
| pok.27.5a | 2026 | SSB | tonnes | Frel | 3 |
| sol.27.20-24 | 2026 | SSB | tonnes | F | 1 |
| sol.27.7a | 2026 | SSB | tonnes | F | 1 |
| sol.27.7d | 2026 | SSB | tonnes | F | 1 |
| sol.27.7e | 2026 | SSB | tonnes | F | 2 |
| sol.27.7fg | 2026 | SSB | tonnes | F | 1 |
| sol.27.8ab | 2026 | NA | tonnes | F | 2 |
| spr.27.22-32 | 2024 | SSB | tonnes | F | 1 |
| spr.27.22-32 | 2026 | SSB | tonnes | F | 1 |
| tur.27.4 | 2026 | SSB | tonnes | F | 1 |
| usk.27.5a14 | 2026 | SSB | NA | F | 1 |
| whb.27.1-91214 | 2026 | SSB | tonnes | F | 1 |
| whg.27.47d | 2026 | SSB | tonnes | F | 0 |
| whg.27.7b-ce-k | 2026 | SSB | thousands | F | 0 |
| wit.27.3a47d | 2026 | SSB | tonnes | F | 1 |
