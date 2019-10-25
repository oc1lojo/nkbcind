nkbcShiny <- function(nkbcind, inca = TRUE, ...) {
  if (inca) {
    rccShiny2(
      inca = TRUE,
      incaScript = file.path(Sys.getenv("ScriptPath"), "Stockholm", "Brostcancer", "nkbc-rccShiny-dm.R"),
      idOverviewLink = "PatientOversikt",
      idAuthorisedToView = "registerpostbehorighet",
      outcome = outcome(nkbcind),
      outcomeTitle = outcome_title(nkbcind),
      periodDateLevel = "quarter", # OBS Annorlunda jfr med publika rapporter
      periodLabel = "Kvartal för diagnos",
      periodDefaultStart = paste0(year(today()) - 1, "Q1"),
      periodDefaultEnd = paste0(year(today()) - 1, "Q4"),
      textBeforeSubtitle = textBeforeSubtitle(nkbcind),
      description = description(nkbcind, year(today())),
      varOther = varOther(nkbcind),
      propWithinUnit = ifelse(!is.null(prop_within_unit(nkbcind)), prop_within_unit(nkbcind), "dagar"), # work-around, använd standardvärde
      propWithinValue = ifelse(!is.null(prop_within_value(nkbcind)), prop_within_value(nkbcind), 30), # work-around, använd standardvärde
      targetValues = target_values(nkbcind)
    )
  }
}
