# Jfr https://bitbucket.org/cancercentrum/nkbc-onlinerapporter/src/2018-05-19/nkbc36.R#lines-193

#' @export
nkbc36 <- list(
  code = "nkbc36",
  kortnamn = "nkbc_tackning_for_op_36",
  lab = "Täckningsgrad för rapportering av operation",
  pop = "anmälda fall utan fjärrmetastas vid diagnos där planerad åtgärd är primär operation, preoperativ onkologisk behandling eller konservativ behandling",
  pop_short = "fall med planerad operation (eller konservativ behandling) och utan fjärrmetastas vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Split av anmälan och op formulär okt 2017
      lubridate::year(a_diag_dat) >= 2018,

      # Planerad åtgräd är primär operation, preoperativ onkologisk behandling eller konservativ behandling
      a_planbeh_typ_Varde %in% c(1, 2),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = ifelse(!is.na(op_inr_dat) | !is.na(op_inr_enh), TRUE, FALSE)
    )
  },
  sjhkod_var = "d_opans_sjhkod",
  other_vars = c("a_pat_alder", "d_a_planbeh_typ"),
  om_indikatorn = "Rapportering av operationsuppgifter sker på ett eget formulär till kvalitetsregistret, separat från anmälan.",
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc36) <- "nkbcind"
