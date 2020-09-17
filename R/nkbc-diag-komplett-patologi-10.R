#' @export
nkbc10 <- list(
  code = "nkbc10",
  kortnamn = "nkbc_diag_komplett_patologi_10",
  lab = c(
    sv = "Fullständig patologirapport (innehållande grad, ER, PR, HER2, Ki67)",
    en = "Complete pathology report (including all standard biomarkers)"
  ),
  lab_short = c(
    sv = "Fullständig patologirapport",
    en = "Complete pathology report"
  ),
  pop = c(
    sv = "primärt opererade invasiva fall utan fjärrmetastaser vid diagnos",
    en = "primarly operated invasive cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast opererade
      !is.na(op_kir_dat),

      # Endast primär opereration (planerad om utförd ej finns)
      d_prim_beh_Varde == 1,

      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer",

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      d_op_nhgok = op_pad_nhg_Varde %in% c(1, 2, 3),
      d_op_erok = op_pad_er_Varde %in% c(1, 2) | !is.na(op_pad_erproc),
      d_op_prok = op_pad_pr_Varde %in% c(1, 2) | !is.na(op_pad_prproc),
      d_op_herok = op_pad_her2_Varde %in% c(1, 2, 3) | op_pad_her2ish_Varde %in% c(1, 2),
      # Ki67 tillkom som nationell variabel 2014
      d_op_ki67ok = (op_pad_ki67_Varde %in% c(1, 2, 3) | !is.na(op_pad_ki67proc)) | lubridate::year(a_diag_dat) <= 2013,

      outcome = d_op_nhgok & d_op_erok & d_op_prok & d_op_herok & d_op_ki67ok
    )
  },
  target_values = c(95, 98),
  sjhkod_var = "op_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = list(
    sv = paste(
      "Patologirapporten grundas i mikroskopiska vävnadsanalyser.",
      "Biomarkörerna utgör grunden för beslut om den postoperativa onkologiska behandlingen av bröstcancer (endokrin-, cytostatika- och antikroppsbehandling)."
    ),
    en = paste(
      "The pathology report is based on microscopical analysis of the surgical specimen.",
      "The biomarkers form the basis for decisions on the postoperative oncological treatment of breast cancer (endocrine, chemotherapy and antibody treatment)."
    )
  ),
  vid_tolkning = list(
    sv = "Ki67 tillkom som nationell variabel 2014 och ingår ej i beräkningen före detta år.",
    en = "Ki67 was added as a variable with national coverage in 2014, and is not included in the calculation of proportion with complete pathology report before this year."
  ),
  teknisk_beskrivning = NULL
)
class(nkbc10) <- "nkbcind"
