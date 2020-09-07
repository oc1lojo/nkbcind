nkbc26 <- list(
  code = "nkbc26",
  kortnamn = "nkbc_kir_sentinel_node_26",
  lab = "Sentinel node operation",
  pop = "invasiva fall utan spridning till lymfkörtlar (klinisk diagnos) eller fjärrmetastaser vid diagnos",
  pop_short = "invasiva fall utan spridning till lymfkörtlar eller fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer",

      # Klinisk N0
      a_tnm_nklass_Varde == 0,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = dplyr::case_when(
        op_kir_axilltyp_Varde == 1 ~ 1L,
        op_kir_axilltyp_Varde == 2 ~ 0L,
        op_kir_axilltyp_Varde == 3 ~ 1L,
        op_kir_axilltyp_Varde == 4 ~ 0L,
        op_kir_axilltyp_Varde == 98 ~ NA_integer_,
        TRUE ~ NA_integer_
      ),
      outcome = as.logical(ifelse(op_kir_axill_Varde %in% 0, 0, outcome))
    )
  },
  target_values = c(90, 95),
  sjhkod_var = "op_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn =
    paste(
      "Kännedom om tumörspridning till axillens lymfkörtlar vägleder behandlingsrekommendationer.",
      "Sentinelnodetekniken minskar risken för armbesvär då endast ett fåtal (1–4) körtlar tas bort."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc26) <- "nkbcind"
