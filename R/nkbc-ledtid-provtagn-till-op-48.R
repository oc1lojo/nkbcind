#' @export
nkbc48 <- list(
  code = "nkbc48",
  kortnamn = "nkbc_ledtid_provtagn_till_op_48",
  lab = c(
    sv = "Provtagningsdatum till operation",
    en = "Biopsy date to surgery"
  ),
  pop = c(
    sv = "primärt opererade fall utan fjärrmetastaser vid diagnos",
    en = "primarily operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast primär opereration (planerad om utförd ej finns)
      d_prim_beh_Varde == 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.numeric(lubridate::ymd(op_kir_dat) - lubridate::ymd(a_diag_dat)),
      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 28,
  target_values = 80,
  period_dat_var = "op_kir_dat",
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Handläggningstiden från provtagning som ger cancerdiagnos till operation bör vara kort och oberoende av var patienten söker vård.",
      "Indikatorn provtagningsdatum till operation har funnits sedan registrets start och före införandet av standardiserade vårdförlopp.",
      "Både ledtidens start och slut är tydliga och väl definierade vilket underlättar vid jämförelse."
    ),
    en = paste(
      "The processing time from tissue biopsy and confirmation of cancer diagnosis to surgery should be short and independent of where the patient seeks care.",
      "Both the start and end of the lead time are clear and well defined, which facilitates comparison."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc48) <- "nkbcind"
