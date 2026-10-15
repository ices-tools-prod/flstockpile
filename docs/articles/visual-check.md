# Visual check against SAG

A page to scroll through, to check every stock assessment in the dataset
by eye against the official results in the ICES Stock Assessment Graphs
database, [SAG](https://sg.ices.dk). The stocks are grouped by expert
group. It is run against the live databases each time this site is
built.

For each stock there are

- a **heading** that is a link to the record of the stock assessment in
  SAG, where it has one;
- **labels** for the four summary quantities: green where the dataset
  and SAG are the same in every year, red where they are not, with the
  number of years that differ and, if the whole series is out, the
  median over the years of the dataset value divided by the SAG value,
  and grey where the quantity could not be compared. A year is the same
  if the two values are within 2.5% of each other, or within the
  rounding of the value in SAG;
- **dots** showing which data are present, for the observed data and for
  the model estimates: a filled dot where the dataset has values, a ring
  where it has none;
- **plots** of the four quantities by year, with the dataset as lines,
  green for the model estimates and orange for the observed data, and
  SAG as points. Where the two lines are the same only the green one
  shows. A panel is tinted red where the dataset and SAG differ. Where
  SAG has no catches, its landings and discards are added together and
  shown instead, or its landings alone if it has no discards.

How the summary is calculated, and the usual reasons for a difference,
are in [Comparing the dataset with
SAG](https://ices-tools-prod.github.io/flstockpile/articles/compare-with-sag.md).

Code

``` r

library(flstockpile)
library(FLCore)
library(icesSAG)
library(icesSD)
library(ggplot2)

options(flstockpile.url = "https://github.com/ices-tools-prod/flstockpile/releases/latest/download")

quantities <- c("SSB", "F", "recruitment", "catches")
slots <- c(
  "catch", "catch.n", "catch.wt", "landings", "landings.n", "landings.wt",
  "discards", "discards.n", "discards.wt", "stock", "stock.n", "stock.wt",
  "m", "mat", "harvest", "harvest.spwn", "m.spwn"
)
colours <- c(observed = "#d95f02", estimated = "#1b9e77")
```

Code

``` r

# the stock assessments, with the species of each from the stock database
stocks <- stockpile_stocks()

species <- lapply(unique(stocks$assessment_year), function(year) {
  sd <- getSD(year = year)
  data.frame(stock_code = sd$StockKeyLabel, assessment_year = year, species = sd$SpeciesCommonName)
})
stocks <- merge(stocks, do.call(rbind, species), all.x = TRUE)
stocks <- stocks[order(stocks$expert_group, stocks$stock_code, stocks$assessment_year), ]
stocks$assessment <- paste0(stocks$stock_code, " (", stocks$assessment_year, ")")

# the heading of each stock assessment, a link to its record in SAG
stocks$heading <- ifelse(
  is.na(stocks$assessment_key),
  stocks$assessment,
  paste0("[", stocks$assessment, "](https://standardgraphs.ices.dk/ViewCharts.aspx?key=", stocks$assessment_key, ")")
)

# The summary of a stock assessment in the dataset: SSB, F, recruitment and
# catches, from the model estimates and from the observed data.
#
# Returns a data frame with one row per type, quantity and year.

dataset_summary <- function(stock) {
  types <- names(colours)[c(stock$observed, stock$estimated)]
  rows <- lapply(types, function(type) {
    object <- stockpile_flstock(stock$stock_code, stock$assessment_year, type = type)
    found <- list(catches = catch(object))
    # a stock from SAG has only the catches, landings and discards, with no ages
    if (stock$origin != "SAG") {
      found <- c(list(SSB = ssb(object), F = fbar(object), recruitment = rec(object)), found)
    }
    rows <- lapply(names(found), function(quantity) {
      values <- as.data.frame(found[[quantity]])
      data.frame(type = type, quantity = quantity, year = values$year, dataset = values$data)
    })
    rows <- do.call(rbind, rows)

    # with no stock numbers the SSB is calculated as zero, when it is not known
    no_numbers <- dimnames(stock.n(object))$year[apply(is.na(stock.n(object)), 2, all)]
    rows[!(rows$quantity == "SSB" & rows$year %in% no_numbers), ]
  })
  rows <- do.call(rbind, rows)
  rows$quantity <- factor(rows$quantity, quantities)
  rows[!is.na(rows$dataset), ]
}

# The summary of a stock assessment in SAG, in the same layout.

sag_summary <- function(stock) {
  sag <- getSAG(stock$stock_code, stock$assessment_year)
  # SAG can have more than one assessment of a stock in a year
  sag <- sag[sag$AssessmentKey == stock$assessment_key, ]

  # where SAG has no catches, the landings and discards are used, or the
  # landings alone if there are no discards
  sag$catches <- ifelse(
    !is.na(sag$catches), sag$catches,
    ifelse(!is.na(sag$discards), sag$landings + sag$discards, sag$landings)
  )

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
  sag <- if (is.na(stock$assessment_key)) NULL else try(sag_summary(stock), silent = TRUE)
  if (is.null(sag) || inherits(sag, "try-error") || all(is.na(sag$sag))) {
    return("Not compared: the stock assessment was not found in SAG.")
  }
  lines <- dataset_summary(stock)

  # SAG is compared with the estimates of SSB, F and recruitment, and with
  # the observed catches if there are any
  compared <- ifelse(lines$quantity == "catches" & stock$observed, "observed", "estimated")
  both <- merge(lines[lines$type == compared, c("quantity", "year", "dataset")], sag, all = TRUE)
  both$quantity <- factor(both$quantity, quantities)
  both <- both[!is.na(both$dataset) | !is.na(both$sag), ]

  # both sets of lines are kept, for the plots
  attr(both, "lines") <- lines
  both
})

# Is a value in the dataset the same as the value in SAG?
#
# It is if the two are within 2.5% of each other, or within the rounding of
# SAG: half a unit in the last decimal place that SAG holds for the quantity.

same_as_sag <- function(dataset, sag) {
  places <- 0
  while (places < 6 && any(abs(sag - round(sag, places)) > 1e-9)) places <- places + 1
  abs(dataset / sag - 1) < 0.025 | abs(dataset - sag) <= 0.5 * 10^-places
}

# Is each quantity the same in the dataset and in SAG?
#
# Returns a data frame with one row per quantity: the status, the median of
# the dataset value divided by the SAG value, the number of years compared
# and the number of them that differ.

agreement <- function(check) {
  rows <- lapply(quantities, function(quantity) {
    rows <- if (is.data.frame(check)) {
      check[check$quantity == quantity & !is.na(check$dataset) & !is.na(check$sag) & check$sag != 0, ]
    }
    same <- if (NROW(rows) > 0) same_as_sag(rows$dataset, rows$sag) else logical()
    data.frame(
      quantity = factor(quantity, quantities),
      status = if (length(same) == 0) "not compared" else if (all(same)) "same" else "different",
      ratio = if (length(same) > 0) median(rows$dataset / rows$sag) else NA,
      years = length(same),
      different = sum(!same)
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
  # how many years differ, and by how much if the whole series is out
  text <- ifelse(
    agreed$status == "different",
    paste0(
      agreed$quantity, ": ", agreed$different, " of ", agreed$years, " years",
      ifelse(abs(agreed$ratio - 1) < 0.025, "", paste0(", &times;", signif(agreed$ratio, 3)))
    ),
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
  # the observed lines are drawn first, so the estimates are on top
  lines <- attr(check, "lines")
  lines <- lines[order(lines$type != "observed"), ]
  lines$type <- factor(lines$type, names(colours))

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
    geom_line(aes(y = dataset, colour = type), data = lines, linewidth = 0.7) +
    scale_colour_manual(values = colours, guide = "none") +
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

### [cod.27.1-2.coastN (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22637)

Cod

SSB F recruitment: 32 of 32 years, ×1.83 catches: 1 of 32 years

![Dots showing which data are present for cod.27.1-2.coastN
(2026).](visual-check_files/figure-html/dots-1-1.png)

![SSB, F, recruitment and catches of cod.27.1-2.coastN (2026) by year,
in the dataset and in
SAG.](visual-check_files/figure-html/plots-1-1.png)

## HAWG

### [her.27.20-24 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=21294)

Herring

SSB F recruitment catches: 17 of 35 years

![Dots showing which data are present for her.27.20-24
(2026).](visual-check_files/figure-html/dots-2-1.png)

![SSB, F, recruitment and catches of her.27.20-24 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-2-1.png)

### [her.27.6aS7bc (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=21289)

Herring

SSB F recruitment catches: 14 of 69 years

![Dots showing which data are present for her.27.6aS7bc
(2026).](visual-check_files/figure-html/dots-3-1.png)

![SSB, F, recruitment and catches of her.27.6aS7bc (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-3-1.png)

### [san.sa.1r (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=21280)

Sandeel

SSB F recruitment catches

![Dots showing which data are present for san.sa.1r
(2026).](visual-check_files/figure-html/dots-4-1.png)

Not compared: the stock has more than one season, unit or area.

### [san.sa.2r (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=21281)

Sandeel

SSB F recruitment catches

![Dots showing which data are present for san.sa.2r
(2026).](visual-check_files/figure-html/dots-5-1.png)

Not compared: the stock has more than one season, unit or area.

### [san.sa.3r (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=21282)

Sandeel

SSB F recruitment catches

![Dots showing which data are present for san.sa.3r
(2026).](visual-check_files/figure-html/dots-6-1.png)

Not compared: the stock has more than one season, unit or area.

### [san.sa.4 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=21271)

Sandeel

SSB F recruitment catches

![Dots showing which data are present for san.sa.4
(2026).](visual-check_files/figure-html/dots-7-1.png)

![SSB, F, recruitment and catches of san.sa.4 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-7-1.png)

## NIPAG

### [pra.27.3a4a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22618)

Northern shrimp

SSB F recruitment catches

![Dots showing which data are present for pra.27.3a4a
(2026).](visual-check_files/figure-html/dots-8-1.png)

Not compared: the stock has more than one season, unit or area.

## NWWG

### [caa.27.5a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22633)

Wolffish

SSB F recruitment catches

![Dots showing which data are present for caa.27.5a
(2026).](visual-check_files/figure-html/dots-9-1.png)

![SSB, F, recruitment and catches of caa.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-9-1.png)

### [cod.21.1.isc (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22467)

Cod

SSB F recruitment catches

![Dots showing which data are present for cod.21.1.isc
(2026).](visual-check_files/figure-html/dots-10-1.png)

![SSB, F, recruitment and catches of cod.21.1.isc (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-10-1.png)

### [cod.21.1.osc (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22541)

Cod

SSB F recruitment catches

![Dots showing which data are present for cod.21.1.osc
(2026).](visual-check_files/figure-html/dots-11-1.png)

![SSB, F, recruitment and catches of cod.21.1.osc (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-11-1.png)

### [cod.21.27.1.14 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22425)

Cod

SSB F recruitment catches

![Dots showing which data are present for cod.21.27.1.14
(2026).](visual-check_files/figure-html/dots-12-1.png)

![SSB, F, recruitment and catches of cod.21.27.1.14 (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-12-1.png)

### [had.27.5a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22494)

Haddock

SSB F: 45 of 47 years, ×1.34 recruitment catches

![Dots showing which data are present for had.27.5a
(2026).](visual-check_files/figure-html/dots-13-1.png)

![SSB, F, recruitment and catches of had.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-13-1.png)

### [her.27.5a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22455)

Herring

SSB F: 45 of 46 years, ×1.2 recruitment catches: 4 of 46 years

![Dots showing which data are present for her.27.5a
(2026).](visual-check_files/figure-html/dots-14-1.png)

![SSB, F, recruitment and catches of her.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-14-1.png)

### [lin.27.5a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22475)

Ling

SSB F recruitment catches

![Dots showing which data are present for lin.27.5a
(2026).](visual-check_files/figure-html/dots-15-1.png)

![SSB, F, recruitment and catches of lin.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-15-1.png)

### [ple.27.5a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22531)

Plaice

SSB F recruitment catches

![Dots showing which data are present for ple.27.5a
(2026).](visual-check_files/figure-html/dots-16-1.png)

![SSB, F, recruitment and catches of ple.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-16-1.png)

### [pok.27.5a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22624)

Saithe

SSB F: 44 of 46 years, ×1.14 recruitment catches

![Dots showing which data are present for pok.27.5a
(2026).](visual-check_files/figure-html/dots-17-1.png)

![SSB, F, recruitment and catches of pok.27.5a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-17-1.png)

### [usk.27.5a14 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22530)

Tusk

SSB F recruitment catches

![Dots showing which data are present for usk.27.5a14
(2026).](visual-check_files/figure-html/dots-18-1.png)

![SSB, F, recruitment and catches of usk.27.5a14 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-18-1.png)

## WGBFAS

### [her.27.3031 (2024)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=19122)

Herring

SSB F recruitment catches

![Dots showing which data are present for her.27.3031
(2024).](visual-check_files/figure-html/dots-19-1.png)

![SSB, F, recruitment and catches of her.27.3031 (2024) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-19-1.png)

### [her.27.3031 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22377)

Herring

SSB F recruitment catches

![Dots showing which data are present for her.27.3031
(2026).](visual-check_files/figure-html/dots-20-1.png)

![SSB, F, recruitment and catches of her.27.3031 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-20-1.png)

### [ple.27.21-32 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22470)

Plaice

SSB: 5 of 24 years F: 8 of 24 years recruitment: 9 of 24 years catches:
23 of 23 years, ×1.31

![Dots showing which data are present for ple.27.21-32
(2026).](visual-check_files/figure-html/dots-21-1.png)

![SSB, F, recruitment and catches of ple.27.21-32 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-21-1.png)

### [sol.27.20-24 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=21317)

Sole

SSB: 18 of 42 years F recruitment catches: 15 of 42 years

![Dots showing which data are present for sol.27.20-24
(2026).](visual-check_files/figure-html/dots-22-1.png)

![SSB, F, recruitment and catches of sol.27.20-24 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-22-1.png)

### [spr.27.22-32 (2024)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=18621)

Sprat

SSB F recruitment catches: 7 of 50 years

![Dots showing which data are present for spr.27.22-32
(2024).](visual-check_files/figure-html/dots-23-1.png)

![SSB, F, recruitment and catches of spr.27.22-32 (2024) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-23-1.png)

### [spr.27.22-32 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22378)

Sprat

SSB F recruitment catches: 7 of 52 years

![Dots showing which data are present for spr.27.22-32
(2026).](visual-check_files/figure-html/dots-24-1.png)

![SSB, F, recruitment and catches of spr.27.22-32 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-24-1.png)

## WGBIE

### [ank.27.78abd (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23768)

Black-bellied anglerfish

SSB F recruitment: 1 of 40 years catches: 40 of 40 years, ×1.19

![Dots showing which data are present for ank.27.78abd
(2026).](visual-check_files/figure-html/dots-25-1.png)

![SSB, F, recruitment and catches of ank.27.78abd (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-25-1.png)

### [ank.27.8c9a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23769)

Black-bellied anglerfish

SSB: 47 of 47 years, ×1.61 F recruitment catches: 12 of 46 years

![Dots showing which data are present for ank.27.8c9a
(2026).](visual-check_files/figure-html/dots-26-1.png)

![SSB, F, recruitment and catches of ank.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-26-1.png)

### [hke.27.3a46-8abd (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22627)

Hake

SSB F recruitment catches: 16 of 48 years

![Dots showing which data are present for hke.27.3a46-8abd
(2026).](visual-check_files/figure-html/dots-27-1.png)

![SSB, F, recruitment and catches of hke.27.3a46-8abd (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-27-1.png)

### [hke.27.8c9a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23751)

Hake

SSB F recruitment catches: 15 of 66 years

![Dots showing which data are present for hke.27.8c9a
(2026).](visual-check_files/figure-html/dots-28-1.png)

![SSB, F, recruitment and catches of hke.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-28-1.png)

### meg.27.7b-k8abd (2026)

Megrim

SSB F recruitment catches

![Dots showing which data are present for meg.27.7b-k8abd
(2026).](visual-check_files/figure-html/dots-29-1.png)

Not compared: the stock assessment was not found in SAG.

### [mon.27.8c9a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22608)

White anglerfish

SSB F recruitment catches: 12 of 46 years

![Dots showing which data are present for mon.27.8c9a
(2026).](visual-check_files/figure-html/dots-30-1.png)

![SSB, F, recruitment and catches of mon.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-30-1.png)

### [oal.27.8c9a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22600)

Sole

SSB F recruitment catches

![Dots showing which data are present for oal.27.8c9a
(2026).](visual-check_files/figure-html/dots-31-1.png)

![SSB, F, recruitment and catches of oal.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-31-1.png)

### [sol.27.8ab (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22607)

Sole

SSB F recruitment catches: 8 of 42 years

![Dots showing which data are present for sol.27.8ab
(2026).](visual-check_files/figure-html/dots-32-1.png)

![SSB, F, recruitment and catches of sol.27.8ab (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-32-1.png)

### [sos.27.8c9a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23772)

Sole

SSB F recruitment catches

![Dots showing which data are present for sos.27.8c9a
(2026).](visual-check_files/figure-html/dots-33-1.png)

![SSB, F, recruitment and catches of sos.27.8c9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-33-1.png)

## WGCSE

### [bss.27.6a7bj (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22635)

Seabass

SSB F recruitment catches

![Dots showing which data are present for bss.27.6a7bj
(2026).](visual-check_files/figure-html/dots-34-1.png)

![SSB, F, recruitment and catches of bss.27.6a7bj (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-34-1.png)

### [cod.27.6b (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22684)

Cod

SSB F recruitment catches

![Dots showing which data are present for cod.27.6b
(2026).](visual-check_files/figure-html/dots-35-1.png)

![SSB, F, recruitment and catches of cod.27.6b (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-35-1.png)

### [had.27.6b (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=24779)

Haddock

SSB F recruitment catches: 31 of 35 years, ×0.975

![Dots showing which data are present for had.27.6b
(2026).](visual-check_files/figure-html/dots-36-1.png)

![SSB, F, recruitment and catches of had.27.6b (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-36-1.png)

### [had.27.7a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22672)

Haddock

SSB F recruitment catches

![Dots showing which data are present for had.27.7a
(2026).](visual-check_files/figure-html/dots-37-1.png)

![SSB, F, recruitment and catches of had.27.7a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-37-1.png)

### [had.27.7b-k (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22561)

Haddock

SSB F recruitment catches: 3 of 33 years

![Dots showing which data are present for had.27.7b-k
(2026).](visual-check_files/figure-html/dots-38-1.png)

![SSB, F, recruitment and catches of had.27.7b-k (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-38-1.png)

### [lez.27.4a6a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23776)

Megrim

SSB: 55 of 55 years, ×36400 F: 54 of 54 years, ×0.169 recruitment
catches: 12 of 54 years

![Dots showing which data are present for lez.27.4a6a
(2026).](visual-check_files/figure-html/dots-39-1.png)

![SSB, F, recruitment and catches of lez.27.4a6a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-39-1.png)

### [ple.27.7bc (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23690)

Plaice

SSB F recruitment catches

![Dots showing which data are present for ple.27.7bc
(2026).](visual-check_files/figure-html/dots-40-1.png)

![SSB, F, recruitment and catches of ple.27.7bc (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-40-1.png)

### [sol.27.7a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22641)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7a
(2026).](visual-check_files/figure-html/dots-41-1.png)

![SSB, F, recruitment and catches of sol.27.7a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-41-1.png)

### [sol.27.7bc (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22670)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7bc
(2026).](visual-check_files/figure-html/dots-42-1.png)

![SSB, F, recruitment and catches of sol.27.7bc (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-42-1.png)

### [sol.27.7e (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22656)

Sole

SSB F recruitment catches: 3 of 57 years

![Dots showing which data are present for sol.27.7e
(2026).](visual-check_files/figure-html/dots-43-1.png)

![SSB, F, recruitment and catches of sol.27.7e (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-43-1.png)

### [sol.27.7fg (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22663)

Sole

SSB F recruitment catches: 30 of 55 years, ×0.967

![Dots showing which data are present for sol.27.7fg
(2026).](visual-check_files/figure-html/dots-44-1.png)

![SSB, F, recruitment and catches of sol.27.7fg (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-44-1.png)

### [sol.27.7h-k (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23719)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7h-k
(2026).](visual-check_files/figure-html/dots-45-1.png)

![SSB, F, recruitment and catches of sol.27.7h-k (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-45-1.png)

### whg.27.6a (2026)

Whiting

SSB F recruitment catches

![Dots showing which data are present for whg.27.6a
(2026).](visual-check_files/figure-html/dots-46-1.png)

Not compared: the stock assessment was not found in SAG.

### [whg.27.7b-ce-k (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23780)

Whiting

SSB F recruitment catches

![Dots showing which data are present for whg.27.7b-ce-k
(2026).](visual-check_files/figure-html/dots-47-1.png)

![SSB, F, recruitment and catches of whg.27.7b-ce-k (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-47-1.png)

## WGDEEP

### [rng.27.5b6712b (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22365)

Roundnose grenadier

SSB F recruitment catches

![Dots showing which data are present for rng.27.5b6712b
(2026).](visual-check_files/figure-html/dots-48-1.png)

![SSB, F, recruitment and catches of rng.27.5b6712b (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-48-1.png)

### [sbr.27.6-8 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22362)

Blackspot seabream

SSB F recruitment catches

![Dots showing which data are present for sbr.27.6-8
(2026).](visual-check_files/figure-html/dots-49-1.png)

![SSB, F, recruitment and catches of sbr.27.6-8 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-49-1.png)

### [sbr.27.9 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23734)

Blackspot seabream

SSB F recruitment catches

![Dots showing which data are present for sbr.27.9
(2026).](visual-check_files/figure-html/dots-50-1.png)

![SSB, F, recruitment and catches of sbr.27.9 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-50-1.png)

### [usk.27.6b (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22363)

Tusk

SSB F recruitment catches

![Dots showing which data are present for usk.27.6b
(2026).](visual-check_files/figure-html/dots-51-1.png)

![SSB, F, recruitment and catches of usk.27.6b (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-51-1.png)

## WGEF

### [por.27.nea (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=26019)

Porbeagle

SSB F recruitment catches

![Dots showing which data are present for por.27.nea
(2026).](visual-check_files/figure-html/dots-52-1.png)

![SSB, F, recruitment and catches of por.27.nea (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-52-1.png)

## WGHANSA

### [hom.27.9a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=23760)

Horse mackerel

SSB F recruitment catches

![Dots showing which data are present for hom.27.9a
(2026).](visual-check_files/figure-html/dots-53-1.png)

![SSB, F, recruitment and catches of hom.27.9a (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-53-1.png)

## WGNEPH

### nep.fu.32 (2026)

Norway lobster

SSB F recruitment catches

![Dots showing which data are present for nep.fu.32
(2026).](visual-check_files/figure-html/dots-54-1.png)

Not compared: the stock assessment was not found in SAG.

## WGNSSK

### [bll.27.3a47de (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22409)

Brill

SSB: 77 of 77 years, ×6160 F: 76 of 76 years, ×0.336 recruitment
catches: 31 of 76 years

![Dots showing which data are present for bll.27.3a47de
(2026).](visual-check_files/figure-html/dots-55-1.png)

![SSB, F, recruitment and catches of bll.27.3a47de (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-55-1.png)

### [had.27.46a20 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22417)

Haddock

SSB F recruitment: 1 of 55 years catches: 40 of 54 years

![Dots showing which data are present for had.27.46a20
(2026).](visual-check_files/figure-html/dots-56-1.png)

![SSB, F, recruitment and catches of had.27.46a20 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-56-1.png)

### [ple.27.420 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22489)

Plaice

SSB F recruitment catches

![Dots showing which data are present for ple.27.420
(2026).](visual-check_files/figure-html/dots-57-1.png)

![SSB, F, recruitment and catches of ple.27.420 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-57-1.png)

### [sol.27.7d (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22546)

Sole

SSB F recruitment catches

![Dots showing which data are present for sol.27.7d
(2026).](visual-check_files/figure-html/dots-58-1.png)

![SSB, F, recruitment and catches of sol.27.7d (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-58-1.png)

### [tur.27.4 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22472)

Turbot

SSB F recruitment catches

![Dots showing which data are present for tur.27.4
(2026).](visual-check_files/figure-html/dots-59-1.png)

![SSB, F, recruitment and catches of tur.27.4 (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-59-1.png)

### [whg.27.47d (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22488)

Whiting

SSB F recruitment: 1 of 49 years catches

![Dots showing which data are present for whg.27.47d
(2026).](visual-check_files/figure-html/dots-60-1.png)

![SSB, F, recruitment and catches of whg.27.47d (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-60-1.png)

### [wit.27.3a47d (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=22534)

Witch

SSB F recruitment catches: 5 of 17 years

![Dots showing which data are present for wit.27.3a47d
(2026).](visual-check_files/figure-html/dots-61-1.png)

![SSB, F, recruitment and catches of wit.27.3a47d (2026) by year, in the
dataset and in SAG.](visual-check_files/figure-html/plots-61-1.png)

## WGWIDE

### [mur.27.67a-ce-k89a (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=25910)

Striped red mullet

SSB F recruitment catches

![Dots showing which data are present for mur.27.67a-ce-k89a
(2026).](visual-check_files/figure-html/dots-62-1.png)

![SSB, F, recruitment and catches of mur.27.67a-ce-k89a (2026) by year,
in the dataset and in
SAG.](visual-check_files/figure-html/plots-62-1.png)

### [whb.27.1-91214 (2026)](https://standardgraphs.ices.dk/ViewCharts.aspx?key=25908)

Blue whiting

SSB F recruitment catches

![Dots showing which data are present for whb.27.1-91214
(2026).](visual-check_files/figure-html/dots-63-1.png)

![SSB, F, recruitment and catches of whb.27.1-91214 (2026) by year, in
the dataset and in SAG.](visual-check_files/figure-html/plots-63-1.png)
