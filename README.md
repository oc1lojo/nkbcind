nkbcind
=======

[![Build
Status](https://travis-ci.com/oc1lojo/nkbcind.svg?branch=master)](https://travis-ci.com/oc1lojo/nkbcind)
[![Build
status](https://ci.appveyor.com/api/projects/status/ebayuxjb2vr1u2vw/branch/master?svg=true)](https://ci.appveyor.com/project/oc1lojo/nkbcind/branch/master)

Verktyg för beräkning av kvalitetsindikatorer och
populationskarakteristik för Nationellt kvalitetsregister från
bröstcancer (NKBC).

Detta R-paket är en **central** plats för definition, implementering och
dokumentation av **beräkning av kvalitetsindikatorer och
populationskarakteristik** från NKBC i **alla** utdata-kanaler.

-   Användning på INCA tillsammans med R-paketet
    [nkbcgeneral](https://cancercentrum.bitbucket.io/nkbcgeneral)
    -   NKBC onlinerapporter innanför inloggning på INCA med R-paketet
        [rccShiny](https://cancercentrum.bitbucket.io/rccshiny)
        -   <https://bitbucket.org/cancercentrum/nkbc-onlinerapporter/>
            (RCC-internt kodförråd)
    -   NKBC Koll på läget (KPL), med R-paketet
        [rccKPL](https://bitbucket.org/cancercentrum/rcckpl)
        -   <https://bitbucket.org/cancercentrum/nkbc-kpl/> (RCC-internt
            kodförråd)
    -   NKBC Vården i siffror, med R-paketet
        [incavis](https://bitbucket.org/cancercentrum/incavis)
        -   <https://bitbucket.org/cancercentrum/nkbc-vis/> (RCC-internt
            kodförråd)
-   Användning lokalt på RCC Stockholm-Gotland tillsammans med R-paketet
    [nkbcgeneral](https://cancercentrum.bitbucket.io/nkbcgeneral)
    -   Framtagande av NKBC Interaktiva Årsrapport med R-paketet
        [rccShiny](https://cancercentrum.bitbucket.io/rccshiny)
        -   <https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny>
            (publikt kodförråd)

Jfr
<https://www.cancercentrum.se/samverkan/vara-uppdrag/statistik/kvalitetsregisterstatistik/>

Installation
------------

``` {.r}
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

remotes::install_bitbucket("cancercentrum/nkbcind")
```

Användning
----------

``` {.r}
library(dplyr)
library(tidyr)
library(lubridate)
library(nkbcgeneral) # https://cancercentrum.bitbucket.io/nkbcgeneral/
library(nkbcind) # https://cancercentrum.bitbucket.io/nkbcind/
```

TODO Lägg till exempel.

Tills vidare:

-   För att skapa en interaktiv rapport så börjar jag alltså med allmän
    bearbetning av NKBC-data för att sedan skapa själva
    Shiny-webbapplikationerna,
    <https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny/src/develop/main.R>
-   ...som använder definitionen (här nkbc01 som exempel)
    <https://bitbucket.org/cancercentrum/nkbcind/src/master/R/nkbc-diag-screening-01.R>
-   ...och metoderna\
    <https://bitbucket.org/cancercentrum/nkbcind/src/master/R/nkbcind-methods.R>
