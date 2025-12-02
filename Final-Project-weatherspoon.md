Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Makenzie Weatherspoon
2025-11-04

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [Fill in first analysis](#fill-in-first-analysis)
  - [Fill in second analysis/plot](#fill-in-second-analysisplot)
- [DISCUSSION](#discussion)
  - [Interpretation - fill in
    analysis](#interpretation---fill-in-analysis)
  - [Interpretation - fill in
    analysis/plot](#interpretation---fill-in-analysisplot)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

West Nile Virus (WNV) is a mosquito-borne disease that infects a wide
range of hosts, including humans and birds. This study investigated the
relationship between mosquito blood meal sources and the presence of WNV
in Salt Lake City, Utah. Using CO2 traps, mosquitoes were collected and
analyzed through PCR and DNA sequencing to identify their vertebrate
blood meal hosts. Data were visualized in R to compare host species
composition at sites with and without WNV detection. Preliminary results
suggest that common urban bird species, such as the house finch, may
play an important role in local WNV amplification.

# BACKGROUND

West Nile Virus (WNV) has been around the world since the early 1900s
and was suspected to have traveled to the United States in 1999 (Sejvar
2003). WNV is primarily spread to different hosts through mosquitoes.
WNV can infect a multitude of organisms including humans, horses, and up
to 25 different mammalian species (Cornell Wildlife Health Lab 2016). In
humans, symptoms of WNV appear as having a fever, headache, body aches,
diarrhea, sore throat, nausea, and or vomiting (Cleaveland Clinic 2025).
The CDC reported over 160 human cases in the United States to WNV in the
year 2024 (CDC 2025). However, WNV primarily affects birds as it is seen
in over 300 species (Hofmeister 2025). Studying the levels of viremia,
the presence of a virus in the bloodstream, within bird populations
across specific geographic regions is a common method used to study
disease transmission and will serve as the focus of this research.

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="templateReport_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

Does the number of West Nile Virus in common local bird species directly
relate to the number of human West Nile Virus? If so, then which bird
species has the highest impact on the levels of WNV in Salt Lake City,
Utah?

## Hypothesis

If increased cases of WNV in birds causes increased cases of human WNV,
then it will be likely due to a common species to residential areas
around Salt Lake City, like House Finches.

## Prediction

If house finches contribute to WNV transmission, then areas with greater
mosquito feeding on finches will correspond to increased WNV prevalence
in local mosquito populations.

# METHODS

Teams working with Dr. Norah Saarman used mosquito collection
techniques, such as CO2 collection chambers, to collect female
mosquitoes around Salt Lake City. DNA from each mosquito individually
was extracted to visualize the DNA from the previous blood meal.
Afterwards, Polymerase Chain Reaction (PCR) and DNA sequencing
techniques were used to determine the DNA sequence of the last blood
meal. The species of the blood meal was then determined using the BLAST
database and DNA sequences collected.

Data visualization and analysis were conducted in R using an R Markdown
workflow to promote reproducibility. The dataset
(bloodmeal_plusWNV_for_BIOL3070.csv) was imported and processed to
summarize host species counts at sites with and without WNV viremia
detection. The R code aggregated counts for each host species, compared
host distributions between WNV-positive and WNV-negative locations, and
produced horizontal bar plots to visually represent these patterns.
Custom color palettes and axis adjustments were implemented for clarity
and consistency across plots. This analytical approach allowed us to
explore potential associations between mosquito blood meal preferences
and the presence of WNV in Salt Lake City, Utah.

## Fill in first analysis

Horizontal plots:

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

<img src="templateReport_files/figure-gfm/horiz-plot-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

## Fill in second analysis/plot

``` r
glm1 <- glm(loc_positives ~ host_House_finch, data = counts_matrix, family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
glm2 <- glm(loc_rate ~ host_House_finch, data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

# DISCUSSION

## Interpretation - fill in analysis

Our analysis compared mosquito blood meal sources from WNV-positive and
WNV-negative trapping locations. We observed that mosquitoes feeding on
house finches were more frequently associated with WNV-positive pools,
suggesting this species may serve as an amplifying host in Salt Lake
City. This aligns with prior studies that identify passerine birds as
key reservoirs for WNV transmission. However, additional sampling is
needed to confirm whether finches are the main contributors or if other
urban species also play significant roles.

## Interpretation - fill in analysis/plot

The linear model highlights a p value of p \< 0.05 which indicates a
statistically significant positive corrilation. This means that the
overall number of house finch blood meals at a specific site increases
the likelyhood of a positive WNV result. With that being said, house
finches can be used as a predictor for WNV amplification in and around
Salt Lake City, Utah.

# CONCLUSION

In summary, this study highlights a possible relationship between
mosquito feeding patterns and local WNV prevalence in Salt Lake City.
The findings suggest that common urban bird species, particularly the
house finch, may contribute to WNV amplification in residential areas.
Further studies integrating seasonal sampling and larger data sets will
be important to confirm these trends and inform mosquito control
strategies.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-11-04.

3.  Sejvar JJ. West nile virus: an historical overview. Ochsner J. 2003
    Summer;5(3):6-10. PMID: 21765761; PMCID: PMC3111838.

4.  West Nile virus. Cornell Wildlife Health Lab. (2016, December 1).
    <https://cwhl.vet.cornell.edu/disease/west-nile-virus#>:~:text=The%20first%20cases%20of%20WNV,%2C%20and%20non%2Dhuman%20primates.

5.  What is West Nile virus?. Cleveland Clinic. (2025, September 12).
    <https://my.clevelandclinic.org/health/diseases/10939-west-nile-virus>

6.  Centers for Disease Control and Prevention. (2025). Historic Data
    (1999-2024). Centers for Disease Control and Prevention. <a
    href="https://www.cdc.gov/west-nile-virus/data-maps/historic-data.html\"
    class="uri">https://www.cdc.gov/west-nile-virus/data-maps/historic-data.html\</a>

7.  Hofmeister, E. (2025, July 17). Are birds the only species that is
    susceptible to West Nile virus infection?. USGS.
    <https://www.usgs.gov/faqs/are-birds-only-species-susceptible-west-nile-virus-infection#>:~:text=Environmental%20Health-,Are%20birds%20the%20only%20species%20that%20is%20susceptible%20to%20West,Learn%20more:%20Vector%2DBorne%20Diseases
