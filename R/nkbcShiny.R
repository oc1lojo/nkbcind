nkbcShiny <- function(nkbcind, inca = TRUE, ...) {
  if (inca) {

    # Standardperiod för de interaktiva rapporterna
    if (!is.null(nkbcind$inkl_beskr_onk_beh) && nkbcind$inkl_beskr_onk_beh) {
      # Standardperiod är två år bakåt i tiden för rapporter om given onk beh pga eftersläpning i rapportering
      default_period_year <- lubridate::year(lubridate::today()) - 2
    } else {
      # Standardperiod är föregående år för övriga rapporter
      default_period_year <- lubridate::year(lubridate::today()) - 1
    }

    rccShiny::rccShiny2(
      inca = TRUE,
      incaScript = file.path(Sys.getenv("ScriptPath"), "Stockholm", "Brostcancer", "nkbc-rccShiny-dm.R"),
      idOverviewLink = "PatientOversikt",
      idAuthorisedToView = "registerpostbehorighet",
      outcome = outcome(nkbcind),
      outcomeTitle = outcome_title(nkbcind),
      periodDateLevel = c("year", "quarter"),
      periodLabel = "Diagnosperiod",
      periodDefaultStart = default_period_year,
      periodDefaultEnd = default_period_year,
      textBeforeSubtitle = textBeforeSubtitle(nkbcind),
      # description = description(nkbcind, lubridate::year(lubridate::today())),
      description = description_inca(nkbcind),
      varOther = varOther(nkbcind),
      propWithinUnit = ifelse(!is.null(prop_within_unit(nkbcind)), prop_within_unit(nkbcind), "dagar"), # work-around, använd standardvärde
      propWithinValue = ifelse(!is.null(prop_within_value(nkbcind)), prop_within_value(nkbcind), 30), # work-around, använd standardvärde
      targetValues = target_values(nkbcind)
    )
  }
}
