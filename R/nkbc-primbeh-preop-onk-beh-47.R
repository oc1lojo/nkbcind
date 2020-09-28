#' @export
nkbc47 <- list(
  code = "nkbc47",
  kortnamn = "nkbc_primbeh_preop_onk_beh_47",
  lab = c(
    sv = "Preoperativ onkologisk behandling",
    en = "Preoperative oncological treatment"
  ),
  pop = c(
    sv = "opererade fall utan fjärrmetastaser vid diagnos",
    en = "operated cases without distant metastasis at diagnosis"
  ),
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
  om_indikatorn = list(
    sv = paste(
      "Preoperativ (neoadjuvant) onkologisk behandling är aktuellt när reduktion av primärtumör eller lymfkörtelmetastaser önskas inför kirurgi och/eller utvärdering av behandlingseffekten med tumören kvar är en fördel.",
      "Tumörstorlek, spridning till lymfkörtlarna liksom biologisk subtyp påverkar val av preoperativ behandling eller ej."
    ),
    en = paste(
      "Preoperative (neoadjuvant) oncological treatment is indicated when shrinkage of primary tumour or lymph node metastasis is desired before surgery and/or evaluation of the treatment effect is desired.",
      "Tumour size, spread to the lymph nodes as well as tumour subtype affect the choice of preoperative treatment or not."
    )
  ),
  vid_tolkning = list(
    sv = "För fall med preoperativ onkologisk behandling är tumörkaraktäristika hämtat från nålsbiopsi innan behandling, i övriga fall från operation.",
    en = "For cases with preoperative oncological treatment, tumour characteristics are taken from needle biopsy before treatment, and otherwise postoperatively from the surgical specimen."
  ),
  inkl_beskr_onk_beh = FALSE, # Använder inte uppgifter från onkologi-formulär
  teknisk_beskrivning = NULL
)
class(nkbc47) <- "nkbcind"
