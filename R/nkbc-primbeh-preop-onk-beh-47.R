nkbc47 <- list(
  code = "nkbc47",
  lab = "Preoperativ onkologisk behandling",
  pop = "opererade fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast opererade
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Preop onk beh
      outcome = dplyr::case_when(
        d_prim_beh_Varde %in% 2 ~ TRUE,
        d_prim_beh_Varde %in% 1 ~ FALSE,
        TRUE ~ NA
      )
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_tstad", "d_nstad", "d_trigrp"),
  om_indikatorn =
    paste(
      "Preoperativ (neoadjuvant) onkologisk behandling är aktuellt när reduktion av primärtumören önskas inför kirurgi och/eller utvärdering av behandlingseffekten med tumören kvar är en fördel.",
      "Tumörstorlek, spridning till lymfkörtlarna liksom biologisk subtyp påverkar val av preoperativ behandling eller ej, liksom typ av preoperativ behandling."
    ),
  vid_tolkning = "För fall med preoperativ onkologisk behandling är östrogenreceptoruttryck hämtat från nålsbiopsi innan behandling, i övriga fall från operation.",
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc47) <- "nkbcind"
