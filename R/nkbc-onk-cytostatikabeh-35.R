nkbc35 <- list(
  code = "nkbc35",
  lab = "Cytostatikabehandling, pre- respektive postoperativt",
  pop = "opererade, invasiva fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer",

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Pre eller postoperativ
      outcome = factor(
        dplyr::case_when(
          post_kemo_Varde == 1 & pre_kemo_Varde == 1 ~ 1,
          pre_kemo_Varde == 1 ~ 0,
          post_kemo_Varde == 1 ~ 2,
          post_kemo_Varde == 0 | pre_kemo_Varde == 0 ~ 3
        ),
        levels = c(0, 1, 2, 3),
        labels = c(
          "Enbart preoperativ",
          "Både pre-och postoperativ",
          "Enbart postoperativ",
          "Ingen"
        )
      )
    )
  },
  sjhkod_var = "d_onk_sjhkod",
  other_vars = c("a_pat_alder", "d_tstad", "d_nstad", "d_trigrp"),
  om_indikatorn = "Pre- eller postoperativ cytostatikabehandling rekommenderas i allmänhet vid bröstcancer med spridning till axillens lymfkörtlar, men även utan lymfkörtelengagemang om tumören har svag hormonell känslighet och/eller då det föreligger riskfaktorer.",
  vid_tolkning =
    c(
      "Tumörstorlek och spridning till lymfkörtlar är kliniskt diagnostiserat.",
      "För fall med preoperativ onkologisk behandling är östrogenreceptoruttryck hämtat från nålsbiopsi innan behandling, i övriga fall från operation."
    ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc35) <- "nkbcind"
