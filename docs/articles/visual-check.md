# Visual check against SAG

A page to scroll through, to check every stock assessment in the dataset
by eye against the official results in the ICES Stock Assessment Graphs
database, [SAG](https://sg.ices.dk). The stocks are grouped by expert
group. It is run against the live databases each time this site is
built.

For each stock there are

- **labels** for the four summary quantities: green where the dataset
  and SAG are within 2.5% of each other in every year, red where they
  are not, with the median over the years of the dataset value divided
  by the SAG value, and grey where the quantity could not be compared;
- **dots** showing which data are present, for the observed data and for
  the model estimates: a filled dot where the dataset has values, a ring
  where it has none;
- **plots** of the four quantities by year, with the dataset as a line
  and SAG as points. A panel is tinted red where the two differ.

How the summary is calculated, and the usual reasons for a difference,
are in [Comparing the dataset with
SAG](https://neilcampbelll.github.io/FLstockpile/articles/compare-with-sag.md).

Code

``` r

library(FLStockpile)
library(FLCore)
library(icesSAG)
library(icesSD)
library(ggplot2)

options(FLStockpile.url = "https://github.com/neilcampbelll/FLstockpile/releases/download/v2026.001")

quantities <- c("SSB", "F", "recruitment", "catches")
slots <- c(
  "catch", "catch.n", "catch.wt", "landings", "landings.n", "landings.wt",
  "discards", "discards.n", "discards.wt", "stock", "stock.n", "stock.wt",
  "m", "mat", "harvest", "harvest.spwn", "m.spwn"
)
colours <- c(observed = "#d95f02", estimated = "#1b9e77")
tolerance <- 0.025
```

Code

``` r

# the stock assessments, with the expert group of each
stocks <- stockpile_stocks()

stock_database <- lapply(unique(stocks$assessment_year), function(year) {
  sd <- getSD(year = year)
  data.frame(
    stock_code = sd$StockKeyLabel, assessment_year = year,
    expert_group = sd$ExpertGroup, species = sd$SpeciesCommonName
  )
})
stocks <- merge(stocks, do.call(rbind, stock_database), all.x = TRUE)
stocks$expert_group[is.na(stocks$expert_group)] <- "Not in the stock database"
stocks <- stocks[order(stocks$expert_group, stocks$stock_code, stocks$assessment_year), ]
stocks$assessment <- paste0(stocks$stock_code, " (", stocks$assessment_year, ")")

# The summary of a stock assessment in the dataset: SSB, F and recruitment
# from the estimates, and catches from the observed data if there are any.

dataset_summary <- function(stock) {
  estimated <- if (stock$estimated) stockpile_flstock(stock$stock_code, stock$assessment_year, type = "estimated")
  observed <- if (stock$observed) stockpile_flstock(stock$stock_code, stock$assessment_year, type = "observed")

  found <- list(catches = catch(if (is.null(observed)) estimated else observed))
  if (!is.null(estimated)) {
    found <- c(list(SSB = ssb(estimated), F = fbar(estimated), recruitment = rec(estimated)), found)
  }
  rows <- lapply(names(found), function(quantity) {
    values <- as.data.frame(found[[quantity]])
    data.frame(quantity = quantity, year = values$year, dataset = values$data)
  })
  do.call(rbind, rows)
}

# The summary of a stock assessment in SAG, in the same layout.

sag_summary <- function(stock) {
  sag <- getSAG(stock$stock_code, stock$assessment_year)
  rows <- lapply(quantities, function(quantity) {
    data.frame(quantity = quantity, year = sag$Year, sag = sag[[quantity]])
  })
  do.call(rbind, rows)
}

# The two summaries of every stock, or the reason there are none.

checks <- lapply(seq_len(nrow(stocks)), function(i) {
  stock <- stocks[i, ]
  if (stock$n_seasons > 1 || stock$n_units > 1 || stock$n_areas > 1) {
    return("Not compared: the stock has more than one season, unit or area.")
  }
  sag <- try(sag_summary(stock), silent = TRUE)
  if (inherits(sag, "try-error") || all(is.na(sag$sag))) {
    return("Not compared: the stock assessment was not found in SAG.")
  }
  both <- merge(dataset_summary(stock), sag, all = TRUE)
  both$quantity <- factor(both$quantity, quantities)
  both[!is.na(both$dataset) | !is.na(both$sag), ]
})

# Is each quantity the same in the dataset and in SAG?
#
# Returns a data frame with one row per quantity: the status, and the median
# of the dataset value divided by the SAG value.

agreement <- function(check) {
  rows <- lapply(quantities, function(quantity) {
    ratio <- if (is.data.frame(check)) {
      rows <- check[check$quantity == quantity & !is.na(check$dataset) & !is.na(check$sag) & check$sag != 0, ]
      rows$dataset / rows$sag
    }
    status <- if (length(ratio) == 0) {
      "not compared"
    } else if (max(abs(ratio - 1)) < tolerance) {
      "same"
    } else {
      "different"
    }
    data.frame(
      quantity = factor(quantity, quantities), status = status,
      ratio = if (length(ratio) > 0) median(ratio) else NA
    )
  })
  do.call(rbind, rows)
}

agreements <- lapply(checks, agreement)
```

## Overview

Every stock assessment and quantity at a glance.

Code

``` r

overview <- do.call(rbind, lapply(seq_along(agreements), function(i) {
  cbind(stocks[i, c("assessment", "expert_group")], agreements[[i]])
}))
status_colours <- c(same = "#a6dba0", different = "#e9695f", "not compared" = "grey92")

ggplot(overview, aes(quantity, assessment, fill = status)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  facet_grid(expert_group ~ ., scales = "free_y", space = "free_y") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = status_colours, breaks = names(status_colours)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.spacing = unit(0.6, "lines")
  )
```

![A grid of stock assessments, grouped by expert group, against the four
summary quantities, coloured by whether the dataset and SAG are the
same, different or could not be
compared.](visual-check_files/figure-html/unnamed-chunk-4-1.png)

Code

``` r

# The labels for a stock, as HTML.

labels_html <- function(agreed) {
  text <- ifelse(
    agreed$status == "different",
    paste0(agreed$quantity, " &times;", signif(agreed$ratio, 3)),
    as.character(agreed$quantity)
  )
  class <- c(same = "qc-same", different = "qc-different", "not compared" = "qc-none")[agreed$status]
  paste0("<p class=\"qc-labels\">", paste0("<span class=\"", class, "\">", text, "</span>", collapse = " "), "</p>")
}

# The dots for a stock: which slots have values, observed and estimated.

dots_plot <- function(stock) {
  values <- stockpile_values(stock$stock_code, stock$assessment_year)
  present <- expand.grid(slot = c(slots, "survey index"), type = names(colours))
  present$found <- paste(present$slot, present$type) %in%
    c(paste(values$slot, values$type), paste(values$slot[values$type == "both"], rep(names(colours), each = sum(values$type == "both"))))
  present$found[present$slot == "survey index"] <- c(stock$indices > 0, FALSE)
  present$slot <- factor(present$slot, c(slots, "survey index"))
  present$type <- factor(present$type, rev(names(colours)))

  ggplot(present, aes(slot, type)) +
    geom_point(data = present[!present$found, ], shape = 1, size = 2.2, colour = "grey80") +
    geom_point(aes(colour = type), data = present[present$found, ], size = 3) +
    # the two parts of a slot name go on two lines, to fit
    scale_x_discrete(position = "top", drop = FALSE, labels = function(slot) gsub("[. ]", "\n", slot)) +
    scale_y_discrete(drop = FALSE) +
    scale_colour_manual(values = colours, guide = "none") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(colour = "grey40"),
      axis.text.y = element_text(colour = "grey20")
    )
}

# The plots for a stock: the four quantities in the dataset and in SAG.

check_plot <- function(check, agreed) {
  # every quantity gets a panel, with or without values
  panels <- data.frame(quantity = factor(quantities, quantities), year = min(check$year), value = 0)

  plot <- ggplot(check, aes(year)) +
    geom_blank(aes(y = value), data = panels)
  if (any(agreed$status == "different")) {
    plot <- plot + geom_rect(
      data = agreed[agreed$status == "different", ], inherit.aes = FALSE,
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#e9695f", alpha = 0.12
    )
  }

  plot +
    geom_line(aes(y = dataset), data = check[!is.na(check$dataset), ], colour = colours["estimated"], linewidth = 0.7) +
    geom_point(aes(y = sag), data = check[!is.na(check$sag), ], colour = "grey25", size = 0.7) +
    facet_wrap(~quantity, nrow = 1, scales = "free_y", drop = FALSE) +
    scale_y_continuous(labels = function(y) format(y, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold")
    )
}
```

## AFWG

### cod.27.1-2.coastN (2026)

Cod

SSB F recruitment ×1.83 catches

![Dots showing which data are present for cod.27.1-2.coastN
(2026).](visual-check_files/figure-html/dots-1-1.png)

![SSB, F, recruitment and catches of cod.27.1-2.coastN (2026) by year,
in the dataset and in
SAG.](visual-check_files/figure-html/plots-1-1.png)

## HAWG

### her.27.20-24 (2026)

Herring

SSB F ×1 recruitment catches ×0.983

![Dots showing which data are present for her.27.20-24
(2026).](visual-check_files/figure-html/dots-2-1.png)

![SSB, F, recruitment and catches of her.27.20-24 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-2-1.png)

### her.27.6aS7bc (2026)

Herring

SSB F recruitment catches ×1

![Dots showing which data are present for her.27.6aS7bc
(2026).](visual-check_files/figure-html/dots-3-1.png)

![SSB, F, recruitment and catches of her.27.6aS7bc (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-3-1.png)

### san.sa.1r (2026)

Sandeel

SSB F recruitment catches

![Dots showing which data are present for san.sa.1r
(2026).](visual-check_files/figure-html/dots-4-1.png)

Not compared: the stock has more than one season, unit or area.

### san.sa.2r (2026)

Sandeel

SSB F recruitment catches

![Dots showing which data are present for san.sa.2r
(2026).](visual-check_files/figure-html/dots-5-1.png)

Not compared: the stock has more than one season, unit or area.

### san.sa.3r (2026)

Sandeel

SSB F recruitment catches

![Dots showing which data are present for san.sa.3r
(2026).](visual-check_files/figure-html/dots-6-1.png)

Not compared: the stock has more than one season, unit or area.

## NIPAG

### pra.27.3a4a (2026)

Northern shrimp

SSB F recruitment catches

![Dots showing which data are present for pra.27.3a4a
(2026).](visual-check_files/figure-html/dots-7-1.png)

Not compared: the stock has more than one season, unit or area.

## NWWG

### caa.27.5a (2026)

Wolffish

SSB F recruitment catches

![Dots showing which data are present for caa.27.5a
(2026).](visual-check_files/figure-html/dots-8-1.png)

![SSB, F, recruitment and catches of caa.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-8-1.png)

### cod.21.1.isc (2026)

Cod

SSB F recruitment catches

![Dots showing which data are present for cod.21.1.isc
(2026).](visual-check_files/figure-html/dots-9-1.png)

![SSB, F, recruitment and catches of cod.21.1.isc (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-9-1.png)

### cod.21.1.osc (2026)

Cod

SSB F recruitment catches

![Dots showing which data are present for cod.21.1.osc
(2026).](visual-check_files/figure-html/dots-10-1.png)

![SSB, F, recruitment and catches of cod.21.1.osc (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-10-1.png)

### had.27.5a (2026)

Haddock

SSB F ×1.34 recruitment catches

![Dots showing which data are present for had.27.5a
(2026).](visual-check_files/figure-html/dots-11-1.png)

![SSB, F, recruitment and catches of had.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-11-1.png)

### her.27.5a (2026)

Herring

SSB F ×1.2 recruitment catches ×1

![Dots showing which data are present for her.27.5a
(2026).](visual-check_files/figure-html/dots-12-1.png)

![SSB, F, recruitment and catches of her.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-12-1.png)

### lin.27.5a (2026)

Ling

SSB F recruitment catches

![Dots showing which data are present for lin.27.5a
(2026).](visual-check_files/figure-html/dots-13-1.png)

![SSB, F, recruitment and catches of lin.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-13-1.png)

### ple.27.5a (2026)

Plaice

SSB F recruitment catches

![Dots showing which data are present for ple.27.5a
(2026).](visual-check_files/figure-html/dots-14-1.png)

![SSB, F, recruitment and catches of ple.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-14-1.png)

### pok.27.5a (2026)

Saithe

SSB F ×1.14 recruitment catches

![Dots showing which data are present for pok.27.5a
(2026).](visual-check_files/figure-html/dots-15-1.png)

![SSB, F, recruitment and catches of pok.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-15-1.png)

### usk.27.5a14 (2026)

Tusk

SSB F recruitment catches

![Dots showing which data are present for usk.27.5a14
(2026).](visual-check_files/figure-html/dots-16-1.png)

![SSB, F, recruitment and catches of usk.27.5a14 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-16-1.png)

## WGBFAS

### her.27.3031 (2024)

Herring

SSB F recruitment catches

![Dots showing which data are present for her.27.3031
(2024).](visual-check_files/figure-html/dots-17-1.png)

![SSB, F, recruitment and catches of her.27.3031 (2024) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-17-1.png)

### her.27.3031 (2026)

Herring

SSB F recruitment catches

![Dots showing which data are present for her.27.3031
(2026).](visual-check_files/figure-html/dots-18-1.png)

![SSB, F, recruitment and catches of her.27.3031 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-18-1.png)

### ple.27.21-32 (2026)

Plaice

SSB ×0.996 F ×0.995 recruitment ×0.995 catches

![Dots showing which data are present for ple.27.21-32
(2026).](visual-check_files/figure-html/dots-19-1.png)

![SSB, F, recruitment and catches of ple.27.21-32 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-19-1.png)

### sol.27.20-24 (2026)

Sole

SSB ×1 F recruitment catches

![Dots showing which data are present for sol.27.20-24
(2026).](visual-check_files/figure-html/dots-20-1.png)

![SSB, F, recruitment and catches of sol.27.20-24 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-20-1.png)

### spr.27.22-32 (2024)

Sprat

SSB F recruitment ×0.001 catches ×0.992

![Dots showing which data are present for spr.27.22-32
(2024).](visual-check_files/figure-html/dots-21-1.png)

![SSB, F, recruitment and catches of spr.27.22-32 (2024) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-21-1.png)

### spr.27.22-32 (2026)

Sprat

SSB F recruitment ×0.001 catches ×0.991

![Dots showing which data are present for spr.27.22-32
(2026).](visual-check_files/figure-html/dots-22-1.png)

![SSB, F, recruitment and catches of spr.27.22-32 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-22-1.png)

## WGBIE

### ank.27.78abd (2026)

Black-bellied anglerfish

SSB F recruitment ×1 catches ×1.11

![Dots showing which data are present for ank.27.78abd
(2026).](visual-check_files/figure-html/dots-23-1.png)

![SSB, F, recruitment and catches of ank.27.78abd (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-23-1.png)

### ank.27.8c9a (2026)

Black-bellied anglerfish

SSB ×1.61 F recruitment catches ×1.01

![Dots showing which data are present for ank.27.8c9a
(2026).](visual-check_files/figure-html/dots-24-1.png)

![SSB, F, recruitment and catches of ank.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-24-1.png)

### hke.27.3a46-8abd (2026)

Hake

SSB F recruitment catches ×1.01

![Dots showing which data are present for hke.27.3a46-8abd
(2026).](visual-check_files/figure-html/dots-25-1.png)

![SSB, F, recruitment and catches of hke.27.3a46-8abd (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-25-1.png)

### hke.27.8c9a (2026)

Hake

SSB F recruitment catches ×0.999

![Dots showing which data are present for hke.27.8c9a
(2026).](visual-check_files/figure-html/dots-26-1.png)

![SSB, F, recruitment and catches of hke.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-26-1.png)

### meg.27.7b-k8abd (2026)

Megrim

SSB F recruitment catches

![Dots showing which data are present for meg.27.7b-k8abd
(2026).](visual-check_files/figure-html/dots-27-1.png)

Not compared: the stock assessment was not found in SAG.

### mon.27.8c9a (2026)

White anglerfish

SSB F recruitment catches ×1.01

![Dots showing which data are present for mon.27.8c9a
(2026).](visual-check_files/figure-html/dots-28-1.png)

![SSB, F, recruitment and catches of mon.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-28-1.png)

### sol.27.8ab (2026)

Sole

SSB F recruitment catches ×1

![Dots showing which data are present for sol.27.8ab
(2026).](visual-check_files/figure-html/dots-29-1.png)

![SSB, F, recruitment and catches of sol.27.8ab (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-29-1.png)

## WGCSE

### had.27.6b (2026)

Haddock

SSB F recruitment catches ×0.975

![Dots showing which data are present for had.27.6b
(2026).](visual-check_files/figure-html/dots-30-1.png)

![SSB, F, recruitment and catches of had.27.6b (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-30-1.png)

### had.27.7b-k (2026)

Haddock

SSB F recruitment catches ×0.997

![Dots showing which data are present for had.27.7b-k
(2026).](visual-check_files/figure-html/dots-31-1.png)

![SSB, F, recruitment and catches of had.27.7b-k (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-31-1.png)

### lez.27.4a6a (2026)

Megrim

SSB ×36400 F ×0.169 recruitment catches ×1

![Dots showing which data are present for lez.27.4a6a
(2026).](visual-check_files/figure-html/dots-32-1.png)

![SSB, F, recruitment and catches of lez.27.4a6a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-32-1.png)

### sol.27.7a (2026)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7a
(2026).](visual-check_files/figure-html/dots-33-1.png)

![SSB, F, recruitment and catches of sol.27.7a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-33-1.png)

### sol.27.7e (2026)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7e
(2026).](visual-check_files/figure-html/dots-34-1.png)

![SSB, F, recruitment and catches of sol.27.7e (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-34-1.png)

### sol.27.7fg (2026)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7fg
(2026).](visual-check_files/figure-html/dots-35-1.png)

![SSB, F, recruitment and catches of sol.27.7fg (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-35-1.png)

### whg.27.6a (2026)

Whiting

SSB F recruitment catches

![Dots showing which data are present for whg.27.6a
(2026).](visual-check_files/figure-html/dots-36-1.png)

Not compared: the stock assessment was not found in SAG.

### whg.27.7b-ce-k (2026)

Whiting

SSB F recruitment catches

![Dots showing which data are present for whg.27.7b-ce-k
(2026).](visual-check_files/figure-html/dots-37-1.png)

![SSB, F, recruitment and catches of whg.27.7b-ce-k (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-37-1.png)

## WGHANSA

### hom.27.9a (2026)

Horse mackerel

SSB F recruitment catches

![Dots showing which data are present for hom.27.9a
(2026).](visual-check_files/figure-html/dots-38-1.png)

![SSB, F, recruitment and catches of hom.27.9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-38-1.png)

## WGNEPH

### nep.fu.32 (2026)

Norway lobster

SSB F recruitment catches

![Dots showing which data are present for nep.fu.32
(2026).](visual-check_files/figure-html/dots-39-1.png)

Not compared: the stock assessment was not found in SAG.

## WGNSSK

### bll.27.3a47de (2026)

Brill

SSB ×6160 F ×0.336 recruitment catches

![Dots showing which data are present for bll.27.3a47de
(2026).](visual-check_files/figure-html/dots-40-1.png)

![SSB, F, recruitment and catches of bll.27.3a47de (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-40-1.png)

### had.27.46a20 (2026)

Haddock

SSB F recruitment ×1 catches ×0.991

![Dots showing which data are present for had.27.46a20
(2026).](visual-check_files/figure-html/dots-41-1.png)

![SSB, F, recruitment and catches of had.27.46a20 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-41-1.png)

### ple.27.420 (2026)

Plaice

SSB F recruitment catches

![Dots showing which data are present for ple.27.420
(2026).](visual-check_files/figure-html/dots-42-1.png)

![SSB, F, recruitment and catches of ple.27.420 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-42-1.png)

### sol.27.7d (2026)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7d
(2026).](visual-check_files/figure-html/dots-43-1.png)

![SSB, F, recruitment and catches of sol.27.7d (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-43-1.png)

### tur.27.4 (2026)

Turbot

SSB F recruitment catches

![Dots showing which data are present for tur.27.4
(2026).](visual-check_files/figure-html/dots-44-1.png)

![SSB, F, recruitment and catches of tur.27.4 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-44-1.png)

### whg.27.47d (2026)

Whiting

SSB F recruitment ×1 catches

![Dots showing which data are present for whg.27.47d
(2026).](visual-check_files/figure-html/dots-45-1.png)

![SSB, F, recruitment and catches of whg.27.47d (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-45-1.png)

### wit.27.3a47d (2026)

Witch

SSB F recruitment catches

![Dots showing which data are present for wit.27.3a47d
(2026).](visual-check_files/figure-html/dots-46-1.png)

![SSB, F, recruitment and catches of wit.27.3a47d (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-46-1.png)

## WGWIDE

### whb.27.1-91214 (2026)

Blue whiting

SSB F recruitment catches

![Dots showing which data are present for whb.27.1-91214
(2026).](visual-check_files/figure-html/dots-47-1.png)

![SSB, F, recruitment and catches of whb.27.1-91214 (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-47-1.png)
