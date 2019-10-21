
<!-- README.md är genererad från README.Rmd. Vänligen redigera den filen. -->

# nkbcind

[![Build
Status](https://travis-ci.org/oc1lojo/nkbcind.svg?branch=master)](https://travis-ci.org/oc1lojo/nkbcind)
[![Build
status](https://ci.appveyor.com/api/projects/status/ebayuxjb2vr1u2vw/branch/master?svg=true)](https://ci.appveyor.com/project/oc1lojo/nkbcind/branch/master)

Planen är att lägga material för beräkning av kvalitetsindikatorer för
NKBC här.

Visionen är att detta R-paket kommer vara en **central** plats för
definition, implementering och dokumentation av beräkning av
kvalitetsindikatorer för NKBC i **alla** utdata-kanaler.

  - Planerad använding på INCA
      - NKBC Koll på läget (KPL), med R-paketet
        [rccKPL](https://bitbucket.org/cancercentrum/rcckpl)
      - NKBC Vården i siffror, med R-paketet
        [incavis](https://bitbucket.org/cancercentrum/incavis)
      - NKBC onlinerapporter innanför inloggning på INCA (snart med
        R-paketet
        [rccShiny](https://bitbucket.org/cancercentrum/rccshiny))
  - Planerad använding lokalt på RCC Stockholm-Gotland
      - Framtagande av NKBC Interaktiv Årsrapport med R-paketet
        [rccShiny](https://bitbucket.org/cancercentrum/rccshiny)
          - <https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny>

Jfr
<https://www.cancercentrum.se/samverkan/vara-uppdrag/statistik/kvalitetsregisterstatistik/>

## Installation

``` r
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_bitbucket("cancercentrum/nkbcind")
```

## Användning

``` r
library(dplyr)
library(tidyr)
library(lubridate)
library(nkbcgeneral)
library(nkbcind)
```

TODO Lägg till exempel.

Tills vidare:

  - För att skapa en interaktiv rapport så börjar jag alltså med allmän
    bearbetning av NKBC-data för att sedan skapa själva
    Shiny-webbapplikationerna,
    <https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny/src/develop/main.R>
  - …som använder definitionen (här nkbc01 som exempel)
    <https://bitbucket.org/cancercentrum/nkbcind/src/master/R/nkbc-diag-screening-01.R>
  - …och metoderna  
    <https://bitbucket.org/cancercentrum/nkbcind/src/master/R/nkbcind-methods.R>
