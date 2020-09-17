#' @export
nkbc13 <- list(
  code = "nkbc13",
  kortnamn = "nkbc_tackning_for_preop_beh_13",
  lab = c(
    sv = "Täckningsgrad för rapportering av preoperativ onkologisk behandling",
    en = "Coverage for reporting postoperative oncological treatment"
  ),
  pop = c(
    sv = "fall utan fjärrmetastaser vid diagnos som opererats efter preoperativ onkologisk behandling",
    en = "cases without distant metastasis at diagnosis operated after preoperative oncological treatment"
  ),
  pop_short = c(
    sv = "fall utan fjärrmetastaser vid diagnos med preoperativ onkologisk behandling",
    en = "cases without distant metastasis at diagnosis with preoperative oncological treatment"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast preoponk behandling (planerad om utförd ej finns)
      d_prim_beh_Varde == 2,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = ifelse(!is.na(pre_inr_dat) | !is.na(pre_inr_enh), TRUE, FALSE)
    )
  },
  target_values = c(70, 85),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "d_onkpreans_sjhkod",
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
class(nkbc13) <- "nkbcind"
