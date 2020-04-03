# Jfr https://bitbucket.org/cancercentrum/nkbc-onlinerapporter/src/2018-05-19/nkbc36.R#lines-193

nkbc36 <- list(
  code = "nkbc36",
  lab = "Täckningsgrad för rapportering av operation",
  pop = "fall med planerad operation utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Split av anmälan och op formulär okt 2017
      lubridate::year(a_diag_dat) >= 2018,

      # Endast planerad op (prim op eller med preop onk behandling)
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
  other_vars = "d_prim_beh",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc36) <- "nkbcind"
