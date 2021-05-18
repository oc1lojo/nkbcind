# nkbcind

[![R-CMD-check](https://github.com/oc1lojo/nkbcind/workflows/R-CMD-check/badge.svg)](https://github.com/oc1lojo/nkbcind/actions)
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

## Installation

``` {.r}
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

remotes::install_bitbucket("cancercentrum/nkbcind")
```

## Användning

``` {.r}
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(nkbcgeneral)
library(nkbcind)
```

Läs in ögonblicksbild av NKBC exporterad från INCA.

``` {.r}
load(
  file.path(Sys.getenv("BRCA_DATA_DIR"), "2021-05-04", "nkbc_nat_avid 2021-05-04 08-25-25.RData")
)
```

Generell förbearbetning av NKBC-data.

``` {.r}
df_main <- df %>%
  mutate(across(where(is.factor), as.character)) %>%
  rename_with(
    str_replace, ends_with("_Värde"),
    pattern = "_Värde", replacement = "_Varde"
  ) %>%
  nkbcgeneral::clean_nkbc_data() %>%
  nkbcgeneral::mutate_nkbc_d_vars() %>%
  mutate(period = lubridate::year(a_diag_dat)) %>%
  filter(period %in% 2008:2020)
```

### Exempel: Screeningupptäckt bröstcancer

``` {.r}
outcomeTitle(nkbc01)
#> $sv
#> [1] "Screeningupptäckt bröstcancer"
textBeforeSubtitle(nkbc01)
#>                                                      sv 
#> "Bland kvinnliga fall i åldrarna 40–74 år vid diagnos."
```

Specifik databearbetning för kvalitetsindikatorn "Screeningupptäckt
bröstcancer" (nkbc01).

``` {.r}
df_tmp <- df_main %>%
  filter_pop(nkbc01)() %>%
  mutate_outcome(nkbc01)()
```

Titta på bearbeatad data.

``` {.r}
df_tmp %>%
  mutate(across(any_of("KON_VALUE"), as.factor)) %>%
  mutate(across(ends_with("_Varde"), as.factor)) %>%
  select(KON_VALUE, a_pat_alder, a_diag_screening_Varde, outcome) %>%
  summary()
#>  KON_VALUE  a_pat_alder    a_diag_screening_Varde  outcome       
#>  2:83651   Min.   :40.00   0   :31327             Mode :logical  
#>            1st Qu.:52.00   1   :51996             FALSE:31327    
#>            Median :62.00   98  :  230             TRUE :51996    
#>            Mean   :59.98   NA's:   98             NA's :328      
#>            3rd Qu.:68.00                                         
#>            Max.   :74.00
```

``` {.r}
df_tmp %>%
  select(period, outcome) %>%
  table(useNA = "ifany")
#>       outcome
#> period FALSE TRUE <NA>
#>   2008  2551 3124   37
#>   2009  2428 3243   39
#>   2010  2389 3832   45
#>   2011  2529 3917   32
#>   2012  2337 4091   27
#>   2013  2352 4265   36
#>   2014  2399 4355   42
#>   2015  2380 4205   19
#>   2016  2392 4098   16
#>   2017  2367 4394   10
#>   2018  2355 4259    9
#>   2019  2477 4454    6
#>   2020  2371 3759   10
```

Jfr <https://statistik.incanet.se/brostcancer/> \> Diagnostik \>
Screeningupptäckt bröstcancer
