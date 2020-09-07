nkbc52 <- list(
  code = "nkbc52",
  kortnamn = "nkbc_tackning_for_dir_52",
  lab = "Täckningsgrad för rapportering av direktrekonstruktion/onkoplastikkirurgi",
  pop = "fall opererade med onkoplastikkirurgi och utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,

      # TODO Lämpligt urval? Formuläret driftsattes i slutet på feb 2020.
      lubridate::year(a_diag_dat) >= 2020,

      # Onkoplastikkirurgi
      op_kir_onkoplastik_Varde %in% 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = ifelse(!is.na(dir_inr_dat) | !is.na(dir_inr_enh), TRUE, FALSE)
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  om_indikatorn = "Rapportering av direktrekonstruktion/onkoplastikkirurgi sker på ett eget formulär till kvalitetsregistret (driftsatt feb 2020), separat från operationsformuläret.",
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc52) <- "nkbcind"
