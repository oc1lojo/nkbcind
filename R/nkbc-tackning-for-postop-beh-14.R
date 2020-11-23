#' @export
nkbc14 <- list(
  code = "nkbc14",
  kortnamn = "nkbc_tackning_for_postop_beh_14",
  lab = c(
    sv = "Täckningsgrad för rapportering av postoperativ onkologisk behandling",
    en = "Coverage for reporting preoperative oncological treatment"
  ),
  pop = c(
    sv = "opererade fall utan fjärrmetastaser vid diagnos",
    en = "operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = ifelse(!is.na(post_inr_dat) | !is.na(post_inr_enh), TRUE, FALSE)
    )
  },
  target_values = c(70, 85),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_onkpostans_sjhkod",
  other_vars_inca = "d_invasiv",
  om_indikatorn = list(
    sv = paste(
      "Rapportering av given onkologisk behandling sker på ett eget formulär till kvalitetsregistret, separat från anmälan.",
      "Rapporteringen sker cirka 1 - 1,5 år efter anmälan."
    ),
    en = paste(
      "Information about given oncological treatment is reported to NKBC on a separate form, separately from the notification form.",
      "The reporting takes place approximately 1 - 1.5 years after notification."
    )
  ),
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc14) <- "nkbcind"
