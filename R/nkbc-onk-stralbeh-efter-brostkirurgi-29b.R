#' @export
nkbc29b <- list(
  code = "nkbc29b",
  kortnamn = "nkbc_onk_stralbeh_efter_brostkirurgi_29b",
  lab = "Strålbehandling efter bröstkirurgi",
  pop = "opererade fall med bröstkirurgi och utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Opererade med bröstkirurgi (fall med enbart axillkirurgi exkluderade)
      op_kir_brost_Varde %in% c(1, 2, 4),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      d_pn = factor(
        dplyr::case_when(
          op_pad_lglusant > 0 & op_pad_lglmetant == 0 ~ "Nej (pN-)",
          (op_pad_snmakrometant == 0 & op_pad_snmikrometant > 0 & op_pad_lglmetant == op_pad_snmikrometant) %in% TRUE ~ "Enbart mikrometastas",
          op_pad_lglmetant > 0 & op_pad_lglmetant <= 3 ~ "1-3 metastaser",
          op_pad_lglmetant >= 4 ~ "=> 4 metastaser",
          TRUE ~ "Uppgift saknas"
        ),
        levels = c("Nej (pN-)", "Enbart mikrometastas", "1-3 metastaser", "=> 4 metastaser", "Uppgift saknas")
      ),
      d_op_kir_brost_kat = factor(
        dplyr::case_when(
          op_kir_brost_Varde %in% 1 ~ 1L,
          op_kir_brost_Varde %in% c(2, 4) ~ 2L,
          TRUE ~ NA_integer_
        ),
        levels = c(1, 2),
        labels = c("Partiellt mastektomi", "Mastektomi")
      ),
      outcome = as.logical(post_rt_Varde)
    )
  },
  sjhkod_var = "post_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv", "d_op_pad_nhg", "d_trigrp", "d_pn", "d_prim_beh", "d_op_kir_brost_kat"),
  om_indikatorn =
    paste(
      "Strålbehandling ges för att minska risken för återfall i det opererade bröstet, på bröstkorgsväggen och regionala lymfkörtelstationer.",
      "Strålbehandling mot bröstet är rutin efter bröstbevarande kirurgi.",
      "Strålbehandling mot armhålan är rutin vid spridning till armhålans lymfkörtlar.",
      "Strålbehandling efter mastektomi ges vid stor tumörutbredning och/eller lymfkörtelengagemang.",
      "Vid betydande samsjuklighet får nyttan med strålbehandling vägas mot potentiella nackdelar."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc29b) <- "nkbcind"
