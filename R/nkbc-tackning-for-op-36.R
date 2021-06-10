# Jfr https://bitbucket.org/cancercentrum/nkbc-onlinerapporter/src/2018-05-19/nkbc36.R#lines-193

#' @export
nkbc36 <- list(
  code = "nkbc36",
  kortnamn = "nkbc_tackning_for_op_36",
  lab = c(
    sv = "Täckningsgrad för rapportering av operation",
    en = "Coverage for reporting surgery"
  ),
  pop = c(
    sv = "anmälda fall utan fjärrmetastas vid diagnos där planerad åtgärd är primär operation eller preoperativ onkologisk behandling",
    en = "reported cases without distant metastasis at diagnosis where the planned action is primary surgery or preoperative oncological treatment"
  ),
  pop_short = c(
    sv = "fall med planerad operation och utan fjärrmetastas vid diagnos",
    en = "cases with planned surgery and without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Split av anmälan och op formulär okt 2017
      lubridate::year(a_diag_dat) >= 2018,

      # version < 2.5 (2021-06-09):
      # Planerad åtgräd är primär operation, preoperativ onkologisk behandling eller konservativ behandling
      a_planbeh_typ_Varde %in% c(1, 2) |
        # version >= 2.5 (2021-06-09):
        # Planerad åtgräd är primär operation eller preoperativ onkologisk behandling
        a_planbeh_typ_Varde %in% c(1, 4, 5),

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
  om_indikatorn = list(
    sv = "Rapportering av operationsuppgifter sker på ett eget formulär till kvalitetsregistret, separat från anmälan.",
    en = "Information about surgery is reported on a separate form in NKBC, separately from the notification form."
  ),
  vid_tolkning = list(
    sv = "Fall med planerad konservativ behandling ingår också i populationen för fall diagnosticerade innan juni 2021.",
    en = "Cases with planned conservative treatment are also included in the population for cases diagnosed before June 2021."
  ),
  teknisk_beskrivning = NULL
)
class(nkbc36) <- "nkbcind"
