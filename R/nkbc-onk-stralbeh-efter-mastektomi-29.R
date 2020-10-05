#' @export
nkbc29 <- list(
  code = "nkbc29",
  kortnamn = "nkbc_onk_stralbeh_efter_mastektomi_29",
  lab = c(
    sv = "Strålbehandling efter mastektomi",
    en = "Radiotherapy after mastectomy"
  ),
  pop = c(
    sv = "invasiva fall med spridning till lymfkörtlarna (fall med enbart mikrometastas exkluderade) och utan fjärrmetastaser vid diagnos, opererade med mastektomi",
    en = "invasive cases  with lymph node metastase (cases with only micrometastasis excluded) and without distant metastasis at diagnosis, operated with mastectomy"
  ),
  pop_short = c(
    sv = "invasiva fall med mastektomi, spridning till lymfkörtlarna och utan fjärrmetastaser vid diagnos",
    en = "invasive cases with mastectomy with lymph node metastasis and without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer",

      # Endast mastektomi och subkutan mastektomi
      op_kir_brost_Varde %in% c(2, 4),

      # Spridning till lymfkörtlar
      op_pad_lglmetant > 0,

      # Ej fall med endast mikrometastas
      !((op_pad_snmakrometant == 0 & op_pad_snmikrometant > 0 & op_pad_lglmetant == op_pad_snmikrometant) %in% TRUE),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.logical(post_rt_Varde)
    )
  },
  target_values = c(70, 85),
  period_dat_var = "a_diag_dat",
  sjhkod_var = "post_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_pn"),
  om_indikatorn = list(
    sv = paste(
      "Då hela bröstet opereras bort (mastektomi) behövs ofta inte strålbehandling.",
      "Vid spridning till lymfkörtlarna (makrometastas) bör dock strålbehandling ges både mot bröstkorgsvägg och lymfkörtlar."
    ),
    en = paste(
      "After mastectomy radiotherapy is indicated in case of axillary lymph node involvement (macrometastasis).",
      "Radiotherapy is then directed to both chest wall and regional lymph nodes."
    )
  ),
  vid_tolkning = list(
    sv =
      "Spridning till lymfkörtlar definieras som metastas >0.2 mm i axillen (fall med enbart mikrometastas är exkluderade).",
    en =
      "A metastatic lymph node is defined as a pathologic lymph node of more than 2 mm in size."
  ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc29) <- "nkbcind"
