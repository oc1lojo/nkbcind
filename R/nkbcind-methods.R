# Se t.ex. http://adv-r.had.co.nz/OO-essentials.html#s3

# TODO Designa om detta rudimentära S3-system för kvalitetsindikatorer i NKBC

# Definiera generiska funktioner ----

#' @export
code <- function(x) UseMethod("code")

#' @export
kortnamn <- function(x) UseMethod("kortnamn")

#' @export
lab <- function(x) UseMethod("lab")

#' @export
lab_short <- function(x) UseMethod("lab_short")

#' @export
lab_short_w_pop <- function(x) UseMethod("lab_short_w_pop")

#' @export
pop <- function(x) UseMethod("pop")

#' @export
pop_short <- function(x) UseMethod("pop_short")

#' @export
filter_pop <- function(x) UseMethod("filter_pop")

#' @export
mutate_outcome <- function(x) UseMethod("mutate_outcome")

#' @export
outcome <- function(x) UseMethod("outcome")

#' @export
prop_within_unit <- function(x) UseMethod("prop_within_unit")

#' @export
prop_within_value <- function(x) UseMethod("prop_within_value")

#' @export
target_values <- function(x) UseMethod("target_values")

#' @export
period_dat_var <- function(x) UseMethod("period_dat_var")

#' @export
sjhkod_var <- function(x) UseMethod("sjhkod_var")

#' @export
geo_units_vars <- function(x) UseMethod("geo_units_vars")

#' @export
other_vars <- function(x) UseMethod("other_vars")

#' @export
other_vars_inca <- function(x) UseMethod("other_vars_inca")

#' @export
outcomeTitle <- function(x, locale, ...) UseMethod("outcomeTitle")

#' @export
textBeforeSubtitle <- function(x, locale, ...) UseMethod("textBeforeSubtitle")

#' @export
description <- function(x, report_end_year, locale, ...) UseMethod("description")

#' @export
description_inca <- function(x, ...) UseMethod("description_inca")

#' @export
varOther <- function(x, varbesk, locale, ...) UseMethod("varOther")

#' @export
varOther_inca <- function(x, varbesk, ...) UseMethod("varOther_inca")

#' @export
kpl_description <- function(x, ...) UseMethod("kpl_description")


# Definiera metoder för klasserna nkbcind och nkbc33 ----

#' @export
code.nkbcind <- function(x) x$code

#' @export
kortnamn.nkbcind <- function(x) x$kortnamn

#' @export
lab.nkbcind <- function(x) x$lab

#' @export
lab_short.nkbcind <- function(x) if (!is.null(x$lab_short)) x$lab_short else lab(x)

#' @export
lab_short_w_pop.nkbcind <- function(x) if (!is.null(x$lab_short_w_pop)) x$lab_short_w_pop else lab_short(x)

#' @export
pop.nkbcind <- function(x) x$pop

#' @export
pop_short.nkbcind <- function(x) if (!is.null(x$pop_short)) x$pop_short else pop(x)

#' @export
filter_pop.nkbcind <- function(x) x$filter_pop

#' @export
mutate_outcome.nkbcind <- function(x) x$mutate_outcome

#' @export
outcome.nkbcind <- function(x) if (!is.null(x$outcome)) x$outcome else "outcome"

#' @export
prop_within_unit.nkbcind <- function(x) x$prop_within_unit

#' @export
prop_within_value.nkbcind <- function(x) x$prop_within_value

#' @export
target_values.nkbcind <- function(x) x$target_values

#' @export
period_dat_var.nkbcind <- function(x) x$period_dat_var

#' @export
sjhkod_var.nkbcind <- function(x) x$sjhkod_var

#' @export
geo_units_vars.nkbcind <- function(x) {
  if (!is.null(x$geo_units_vars)) {
    x$geo_units_vars
  } else {
    c("region", "landsting", "sjukhus")
  }
}

#' @export
other_vars.nkbcind <- function(x) x$other_vars

#' @export
other_vars_inca.nkbcind <- function(x) {
  if (!is.null(x$other_vars_inca)) {
    x$other_vars_inca
  } else {
    other_vars(x)
  }
}

#' @export
outcomeTitle.nkbcind <- function(x, locale = "sv", ...) {
  if (!is.null(x$outcome_title)) {
    x$outcome_title[locale]
  } else {
    as.list(lab(x)[locale])
  }
}

#' @export
textBeforeSubtitle.nkbcind <- function(x, locale = "sv", ...) {
  c(
    sv = paste0("Bland ", pop_short(x)["sv"], "."),
    en = paste0("Among ", pop_short(x)["en"], ".")
  )[locale]
}

#' @export
description.nkbcind <- function(x, report_end_year = report_end_year, locale = "sv", ...) {
  # Lägga till "(andel inom ... dagar)" för kontinuerliga variabler
  if (!is.null(prop_within_value(x))) {
    target_levels_extra_txt_sv <- paste0("Andel inom ", prop_within_value(x), " ", ifelse(!is.null(prop_within_unit(x)), prop_within_unit(x), "dagar"), " ")
  } else {
    target_levels_extra_txt_sv <- NULL
  }

  # Lägga till "for proportion within ... days)" för kontinuerliga variabler
  if (!is.null(prop_within_value(x))) {
    target_levels_extra_txt_en <- paste0("Proportion within ", prop_within_value(x), " days ")
  } else {
    target_levels_extra_txt_en <- NULL
  }

  y <- list(
    sv = c(
      # Om indikatorn
      paste(
        c(
          x$om_indikatorn$sv,
          if (!is.null(x$target_values)) {
            dplyr::case_when(
              length(x$target_values) == 1 ~
              paste0("Målnivå: ", target_levels_extra_txt_sv, x$target_values[1], "%"),
              length(x$target_values) == 2 ~
              paste0("Målnivåer: ", target_levels_extra_txt_sv, x$target_values[1], "% (låg) ", x$target_values[2], "% (hög)")
            )
          }
        ),
        collapse = "\n<p></p>\n"
      ),
      # Vid tolkning
      paste(
        c(
          x$vid_tolkning$sv,
          if (!is.null(x$inkl_beskr_missca) && x$inkl_beskr_missca == TRUE) {
            "Datum för välgrundad misstanke om cancer tillkom som variabel 2016 och innan detta har datum för första kontakt använts."
          },
          if (x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod")) {
            paste(
              "Uppgifterna redovisas uppdelat på sjukhus där onkologisk behandling ges, vilket i vissa fall kan innebära en annan enhet än anmälande/opererande enhet.",
              "När antalet behandlade fall skiljer sig kraftigt från antalet anmälda fall vid en enhet kan starka selektionsmekanismer på basen av patient- och tumördata styra vilken enhet som ger onkologisk behandling."
            )
          },
          if (!is.null(x$inkl_beskr_onk_beh) && x$inkl_beskr_onk_beh == TRUE) {
            paste(
              "Uppgifter som rör given onkologisk behandling redovisas enbart t.o.m.",
              report_end_year - 1, "p.g.a. eftersläpning i rapporteringen."
            )
          },
          if (!is.null(x$inkl_beskr_overlevnad_5ar) && x$inkl_beskr_overlevnad_5ar == TRUE) {
            paste0("Uppgifter som rör 5-årsöverlevnad redovisas enbart t.o.m. ", report_end_year - 5, ".")
          },
          paste(
            "Ett fall per bröst kan rapporterats till Nationellt kvalitetsregister för bröstcancer (NKBC).",
            "Det innebär att samma person kan finnas med i statistiken två gånger."
          ),
          "Skövde och Lidköpings sjukhus presenteras tillsammans som Skaraborg.",
          if (x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod", "d_onkpreans_sjhkod", "d_onkpostans_sjhkod", "d_prim_beh_sjhkod")) {
            "Malmö och Lunds sjukhus presenteras tillsammans som Lund/Malmö."
          }
        ),
        collapse = "\n<p></p>\n"
      ),
      # Teknisk beskrivning
      paste(
        c(
          x$teknisk_beskrivning$sv,
          paste0("Population: ", x$pop["sv"], "."),
          paste0(
            "Uppgifterna redovisas uppdelat på ",
            dplyr::case_when(
              x$sjhkod_var %in% "a_inr_sjhkod" ~
              "anmälande sjukhus",
              x$sjhkod_var %in% "op_inr_sjhkod" ~
              "opererande sjukhus",
              x$sjhkod_var %in% "d_opans_sjhkod" ~
              "opererande sjukhus, och om detta saknas, anmälande sjukhus",
              x$sjhkod_var %in% "d_pat_sjhkod" ~
              "opererande sjukhus för primärt opererade fall, annars anmälande sjukhus",
              x$sjhkod_var %in% "d_prim_beh_sjhkod" ~
              "sjukhus ansvarig för primär behandling",
              x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod") ~
              "sjukhus där onkologisk behandling ges",
              x$sjhkod_var %in% c("d_onkpreans_sjhkod", "d_onkpostans_sjhkod") ~
              "rapporterande sjukhus där onkologisk behandling ges, och om detta saknas, sjukhus ansvarigt för rapportering av onkologisk behandling, sjukhus för onkologisk behandling, anmälande sjukhus"
            ),
            "."
          )
        ),
        collapse = "\n<p></p>\n"
      )
    ),
    en = c(
      # Om indikatorn
      paste(
        c(
          x$om_indikatorn$en,
          if (!is.null(x$target_values)) {
            dplyr::case_when(
              length(x$target_values) == 1 ~
              paste0("Target level: ", target_levels_extra_txt_en, x$target_values[1], "%"),
              length(x$target_values) == 2 ~
              paste0("Target levels: ", target_levels_extra_txt_en, x$target_values[1], "% (low) ", x$target_values[2], "% (high)")
            )
          }
        ),
        collapse = "\n<p></p>\n"
      ),
      # Vid tolkning
      paste(
        c(
          x$vid_tolkning$en,
          if (!is.null(x$inkl_beskr_missca) && x$inkl_beskr_missca == TRUE) {
            "The date of well-founded suspicion of cancer was added as a variable in 2016 and before this date the date of first contact was used."
          },
          if (x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod")) {
            paste(
              "The information is presented per hospital where oncological treatment is given, which in some cases may be a unit other than the notifying unit/unit performing surgery.",
              "When the number of treated cases differs greatly from the number of reported cases at a unit, strong selection mechanisms based on patient and tumor data can direct which unit provides oncological treatment."
            )
          },
          if (!is.null(x$inkl_beskr_onk_beh) && x$inkl_beskr_onk_beh == TRUE) {
            paste(
              "Information relating to given oncological treatment is presented only up to",
              report_end_year - 1, "due to lag in reporting."
            )
          },
          if (!is.null(x$inkl_beskr_overlevnad_5ar) && x$inkl_beskr_overlevnad_5ar == TRUE) {
            paste0("Information relating to 5-year survival is presented only up to ", report_end_year - 5, ".")
          },
          paste(
            "One case per breast can be reported to the National Quality Registry for Breast Cancer (NKBC).",
            "This means that the same person can be included in the statistics twice."
          ),
          "Skövde and Lidköping hospitals are presented as Skaraborg.",
          if (x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod", "d_onkpreans_sjhkod", "d_onkpostans_sjhkod", "d_prim_beh_sjhkod")) {
            "Lund and Malmö hospitals are presented as Lund/Malmö."
          }
        ),
        collapse = "\n<p></p>\n"
      ),
      # Teknisk beskrivning
      paste(
        c(
          x$teknisk_beskrivning_en,
          paste0("Population: ", x$pop["en"], "."),
          paste0(
            "The information is presented per ",
            dplyr::case_when(
              x$sjhkod_var %in% "a_inr_sjhkod" ~
              "notifying hospital",
              x$sjhkod_var %in% "op_inr_sjhkod" ~
              "hospital performing surgery",
              x$sjhkod_var %in% "d_opans_sjhkod" ~
              "hospital performing surgery and, if missing, notifying hospital",
              x$sjhkod_var %in% "d_pat_sjhkod" ~
              "hospital performing surgery for cases with primary surgery, otherwise notifying hospital",
              x$sjhkod_var %in% "d_prim_beh_sjhkod" ~
              "hospital responsible for primary treatment",
              x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod") ~
              "hospital where oncological treatment is given",
              x$sjhkod_var %in% c("d_onkpreans_sjhkod", "d_onkpostans_sjhkod") ~
              "reporting hospital where oncological treatment is given and if missing, hospital responsible for reporting oncological treatment, hospital for oncological treatment, reporting hospital"
            ),
            "."
          )
        ),
        collapse = "\n<p></p>\n"
      )
    )
  )
  return(y[locale])
}

#' @export
description.nkbc33 <- function(x, report_end_year = report_end_year, locale = "sv", ...) {
  # Anpassad för rapporteringa av täckningsgrad mot cancerregistret (nkbc33)

  y <- list(
    sv = c(
      # Om indikatorn
      paste(
        c(
          x$om_indikatorn$sv,
          if (!is.null(x$target_values)) {
            dplyr::case_when(
              length(x$target_values) == 1 ~
              paste0("Målnivå: ", x$target_values[1], "%"),
              length(x$target_values) == 2 ~
              paste0("Målnivåer: ", x$target_values[1], "% (låg) ", x$target_values[2], "% (hög)")
            )
          }
        ),
        collapse = "\n<p></p>\n"
      ),
      # Vid tolkning
      paste(
        c(
          x$vid_tolkning$sv,
          paste(
            "Ett fall per bröst kan rapporterats till Nationellt kvalitetsregister för bröstcancer (NKBC).",
            "Det innebär att samma person kan finnas med i statistiken upp till två gånger."
          )
        ),
        collapse = "\n<p></p>\n"
      ),
      # Teknisk beskrivning
      paste(
        c(
          x$teknisk_beskrivning$sv,
          paste0("Population: ", x$pop["sv"], "."),
          "Uppgifterna redovisas uppdelat på den sjukvårdsregion personen var bosatt i vid diagnos."
        ),
        collapse = "\n<p></p>\n"
      )
    ),
    en = c(
      # Om indikatorn
      paste(
        c(
          x$om_indikatorn$en,
          if (!is.null(x$target_values)) {
            dplyr::case_when(
              length(x$target_values) == 1 ~
              paste0("Target level: ", x$target_values[1], "%"),
              length(x$target_values) == 2 ~
              paste0("Target levels: ", x$target_values[1], "% (low) ", x$target_values[2], "% (high)")
            )
          }
        ),
        collapse = "\n<p></p>\n"
      ),
      # Vid tolkning
      paste(
        c(
          x$vid_tolkning$en,
          paste(
            "One case per breast can be reported to the National Quality Registry for Breast Cancer (NKBC).",
            "This means that the same person can be included in the statistics twice."
          )
        ),
        collapse = "\n<p></p>\n"
      ),
      # Teknisk beskrivning
      paste(
        c(
          x$teknisk_beskrivning$en,
          paste0("Population: ", x$pop["en"], "."),
          "The information is presented per healthcare region in which the person resided at the time of diagnosis"
        ),
        collapse = "\n<p></p>\n"
      )
    )
  )
  return(y[locale])
}

#' @export
description_inca.nkbcind <- function(x, ...) {
  # Lägga till "(andel inom ... dagar)" för kontinuerliga variabler
  if (!is.null(prop_within_value(x))) {
    target_levels_extra_txt_sv <- paste0("Andel inom ", prop_within_value(x), " ", ifelse(!is.null(prop_within_unit(x)), prop_within_unit(x), "dagar"), " ")
  } else {
    target_levels_extra_txt_sv <- NULL
  }

  c(
    # Om indikatorn
    paste(
      c(
        x$om_indikatorn$sv,
        if (!is.null(x$target_values)) {
          dplyr::case_when(
            length(x$target_values) == 1 ~
            paste0("Målnivå: ", target_levels_extra_txt_sv, x$target_values[1], "%"),
            length(x$target_values) == 2 ~
            paste0("Målnivåer: ", target_levels_extra_txt_sv, x$target_values[1], "% (låg) ", x$target_values[2], "% (hög)")
          )
        }
      ),
      collapse = "\n<p></p>\n"
    ),
    # Vid tolkning
    paste(
      c(
        x$vid_tolkning$sv,
        if (!is.null(x$inkl_beskr_missca) && x$inkl_beskr_missca == TRUE) {
          "Datum för välgrundad misstanke om cancer tillkom som variabel 2016 och innan detta har datum för första kontakt använts."
        },
        if (x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod")) {
          paste(
            "Uppgifterna redovisas uppdelat på sjukhus där onkologisk behandling ges, vilket i vissa fall kan innebära en annan enhet än anmälande/opererande enhet.",
            "När antalet behandlade fall skiljer sig kraftigt från antalet anmälda fall vid en enhet kan starka selektionsmekanismer på basen av patient- och tumördata styra vilken enhet som ger onkologisk behandling."
          )
        },
        if (!is.null(x$inkl_beskr_onk_beh) && x$inkl_beskr_onk_beh == TRUE) {
          paste(
            "Rapportering av given onkologisk behandling sker på ett eget formulär till kvalitetsregistret, separat från anmälan.",
            "Rapporteringen sker cirka 1 - 1,5 år efter anmälan."
          )
        },
        # if (!is.null(x$inkl_beskr_overlevnad_5ar) && x$inkl_beskr_overlevnad_5ar == TRUE) {
        #   paste0("Uppgifter som rör 5 års överlevnad redovisas enbart t.o.m. ", report_end_year - 5, ".")
        # },
        paste(
          "Ett fall per bröst kan rapporterats till det nationella kvalitetsregistret för bröstcancer.",
          "Det innebär att samma person kan finnas med i statistiken upp till två gånger."
        )
        # "Skövde och Lidköpings sjukhus presenteras tillsammans som Skaraborg.",
        # if (x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod", "d_onkpreans_sjhkod", "d_onkpostans_sjhkod", "d_prim_beh_sjhkod")) {
        #   "Malmö och Lunds sjukhus presenteras tillsammans som Lund/Malmö."
        # }
      ),
      collapse = "\n<p></p>\n"
    ),
    # Teknisk beskrivning
    paste(
      c(
        x$teknisk_beskrivning$sv,
        paste0("Population: ", x$pop["sv"], "."),
        paste0(
          "Uppgifterna redovisas uppdelat på ",
          dplyr::case_when(
            x$sjhkod_var %in% "a_inr_sjhkod" ~
            "anmälande sjukhus",
            x$sjhkod_var %in% "op_inr_sjhkod" ~
            "opererande sjukhus",
            x$sjhkod_var %in% "d_opans_sjhkod" ~
            "opererande sjukhus, och om detta saknas, anmälande sjukhus",
            x$sjhkod_var %in% "d_pat_sjhkod" ~
            "opererande sjukhus för primärt opererade fall, annars anmälande sjukhus",
            x$sjhkod_var %in% "d_prim_beh_sjhkod" ~
            "sjukhus ansvarig för primär behandling",
            x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod") ~
            "sjukhus där onkologisk behandling ges",
            x$sjhkod_var %in% c("d_onkpreans_sjhkod", "d_onkpostans_sjhkod") ~
            "rapporterande sjukhus där onkologisk behandling ges, och om detta saknas, sjukhus ansvarigt för rapportering av onkologisk behandling, sjukhus för onkologisk behandling, anmälande sjukhus"
          ),
          " eller patientens hemortslän vid diagnos."
        )
      ),
      collapse = "\n<p></p>\n"
    )
  )
}

#' @export
description_inca.nkbc33 <- function(x, ...) {
  # Anpassad för rapporteringa av täckningsgrad mot cancerregistret (nkbc33)
  c(
    # Om indikatorn
    paste(
      c(
        x$om_indikatorn$sv,
        if (!is.null(x$target_values)) {
          dplyr::case_when(
            length(x$target_values) == 1 ~
            paste0("Målnivå: ", x$target_values[1], "%"),
            length(x$target_values) == 2 ~
            paste0("Målnivåer: ", x$target_values[1], "% (låg) ", x$target_values[2], "% (hög)")
          )
        }
      ),
      collapse = "\n<p></p>\n"
    ),
    # Vid tolkning
    paste(
      c(
        x$vid_tolkning$sv,
        paste(
          "Ett fall per bröst kan rapporterats till det nationella kvalitetsregistret för bröstcancer.",
          "Det innebär att samma person kan finnas med i statistiken upp till två gånger."
        )
      ),
      collapse = "\n<p></p>\n"
    ),
    # Teknisk beskrivning
    paste(
      c(
        x$teknisk_beskrivning$sv,
        paste0("Population: ", x$pop, "."),
        "Sjukhus är i första hand inrapporterande sjukhus på anmälan i kvalitetsregistret och om detta saknas remitterande klinik i cancerregistret och om detta saknas arbetskodklinik i cancerregistret."
      ),
      collapse = "\n<p></p>\n"
    )
  )
}

#' @export
varOther.nkbcind <- function(x, varbesk = varbesk_other_vars, locale = "sv", ...) {
  if (is.null(x$other_vars)) {
    return(NULL)
  } else {
    lapply(x$other_vars, function(y) list(var = y, label = varbesk[[y]][locale]))
  }
}

#' @export
varOther_inca.nkbcind <- function(x, varbesk = varbesk_other_vars, ...) {
  if (is.null(x$other_vars_inca)) {
    varOther(x, varbesk = varbesk, ...)
  } else {
    lapply(x$other_vars_inca, function(y) list(var = y, label = varbesk[[y]]["sv"]))
  }
}

#' @export
kpl_description.nkbcind <- function(x, ...) {
  if (!is.null(prop_within_value(x))) {
    # x antas vara en ledtid
    lab_mod <- paste("Tid från", tolower(lab(x)["sv"]), "inom", prop_within_value(x), "dagar")
  } else {
    lab_mod <- lab(x)["sv"] %>%
      stringr::str_replace(", måluppfyllelse", "")
  }

  # Adopted from https://stackoverflow.com/a/56125845
  str_to_lower2 <- function(x) {
    str_keep_upper <- c(
      "ER",
      "ER-positivitet",
      "PR",
      "PR-positivitet",
      "HER2",
      "HER2-positivitet",
      "IHC",
      "ISH",
      "(ISH)",
      "Ki67",
      "NHG"
    )
    paste(lapply(strsplit(x, " "), function(y) ifelse(y %in% str_keep_upper, y, tolower(y)))[[1]], collapse = " ")
  }

  paste(
    c(
      paste0("Andel med ", str_to_lower2(lab_mod), " bland ", pop(x)["sv"], ".") %>%
        # stringr::str_to_sentence(locale = "sv") %>%
        stringr::str_replace_all("min vårdplan", "Min Vårdplan"),
      if (!is.null(x$inkl_beskr_missca) && x$inkl_beskr_missca == TRUE) {
        "Datum för välgrundad misstanke om cancer tillkom som variabel 2016 och innan detta har datum för 1:a kontakt använts."
      },
      paste0(
        "Fall beskrivs utifrån ",
        dplyr::case_when(
          x$period_dat_var %in% "a_diag_dat" ~ "diagnosdatum",
          x$period_dat_var %in% "d_pre_onk_dat" ~ "startdatum för preoperativ onkologisk behandling",
          x$period_dat_var %in% "op_kir_dat" ~ "operationsdatum"
        ),
        " och ",
        dplyr::case_when(
          x$sjhkod_var %in% "a_inr_sjhkod" ~
          "anmälande sjukhus",
          x$sjhkod_var %in% "op_inr_sjhkod" ~
          "opererande sjukhus",
          x$sjhkod_var %in% "d_opans_sjhkod" ~
          "opererande sjukhus, och om detta saknas, anmälande sjukhus",
          x$sjhkod_var %in% "d_pat_sjhkod" ~
          "opererande sjukhus för primärt opererade fall, annars anmälande sjukhus",
          x$sjhkod_var %in% "d_prim_beh_sjhkod" ~
          "sjukhus ansvarig för primär behandling",
          x$sjhkod_var %in% c("post_inr_sjhkod", "pre_inr_sjhkod", "d_onk_sjhkod") ~
          "sjukhus där onkologisk behandling ges",
          x$sjhkod_var %in% c("d_onkpreans_sjhkod", "d_onkpostans_sjhkod") ~
          paste(
            "rapporterande sjukhus där onkologisk behandling ges,",
            "och om detta saknas,",
            "sjukhus ansvarigt för rapportering av onkologisk behandling, sjukhus för onkologisk behandling, anmälande sjukhus"
          )
        ),
        "."
      ),
      if (!is.null(x$target_values)) {
        dplyr::case_when(
          length(x$target_values) == 1 ~
          paste0("Målnivå: ", x$target_values[1], "%."),
          length(x$target_values) == 2 ~
          paste0("Målnivåer: ", x$target_values[1], "% (låg) ", x$target_values[2], "% (hög).")
        )
      }
    ),
    collapse = " "
  )
}
