#' @export
nkbc22 <- list(
  code = "nkbc22",
  kortnamn = "nkbc_ledtid_op_till_cytostatikabeh_22",
  lab = c(
    sv = "Operation till cytostatikabehandling",
    en = "Surgery to chemotherapy"
  ),
  pop = c(
    sv = "primärt opererade fall utan fjärrmetastaser vid diagnos",
    en = "primarily operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

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
      outcome = as.numeric(lubridate::ymd(post_kemo_dat) - lubridate::ymd(op_kir_dat)),
      outcome = ifelse(outcome < 0, 0, outcome)
    )
  },
  prop_within_value = 24,
  target_values = 80,
  sjhkod_var = "post_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Handläggningstiden från operation och PAD-svar till start av postoperativ cytostatikabehandling bör vara rimlig och oberoende av var patienten söker vård.",
      "Ledtidens start och slut är tydliga och väl definierade vilket underlättar vid jämförelse."
    ),
    en = paste(
      "The processing time from surgery and pathology report to the start of postoperative chemotherapy should be kept within reasonable time limit and independent of where the patient seeks care.",
      "The start and end of the lead time are clear and well defined, which facilitates comparison."
    )
  ),
  vid_tolkning = list(
    sv = "Operationsdatum är datum för första operation, vilket innebär att tiden från sista operation till start av cytostatikabehandling kan vara kortare än den som redovisas.",
    en = "The date of surgery is the date of the first operation, which means that the time from the last operation to the start of chemotherapy treatment may be shorter than that reported."
  ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc22) <- "nkbcind"
