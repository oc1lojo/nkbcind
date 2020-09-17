#' @export
nkbc45 <- list(
  code = "nkbc45",
  kortnamn = "nkbc_kir_axillkir_45",
  lab = c(
    sv = "Typ av axillkirurgi",
    en = "Type of axillary surgery"
  ),
  lab_short = c(
    sv = "Axillkirurgi",
    en = "Axillary surgery"
  ),
  pop = c(
    sv = "opererade fall med axillingrepp och utan fjärrmetastaser vid diagnos",
    en = "operated cases with axillary surgery without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Opererade fall
      !is.na(op_kir_dat),

      # Axillingrepp
      op_kir_axill_Varde %in% 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = factor(
        dplyr::if_else(op_kir_axilltyp_Varde %in% c(1, 2, 3), op_kir_axilltyp_Varde, 98L),
        levels = c(1, 3, 2, 98),
        labels = c("Enbart SN", "SN och utrymning", "Enbart utrymning", "Uppgift saknas")
      ),
      outcome_en = factor(
        dplyr::if_else(op_kir_axilltyp_Varde %in% c(1, 2, 3), op_kir_axilltyp_Varde, 98L),
        levels = c(1, 3, 2, 98),
        labels = c("SN only", "SN and axillary clearance", "Axillary clearance only", "Missing")
      )
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv", "d_nstad"),
  other_vars_inca = c("a_pat_alder", "d_invasiv", "d_nstad", "d_prim_beh"),
  om_indikatorn = list(
    sv = c(
      "Axillkirurgi utförs för att klarlägga om spridning finns till armhålans lymfkörtlar.",
      "Förenklat utförs sentinel node biopsi vid kliniskt nodnegativ sjukdom, axillutrymning vid kliniskt nodpositiv sjukdom och SN och utrymning vid kliniskt nodnegativ sjukdom där sentinel node inte hittas eller innehåller metastas."
    ),
    en = c(
      "Axillary surgery is performed to investigate if there is tumour spread to the lymph nodes of the armpit.",
      "In short, sentinel node biopsy is performed in clinically node-negative disease, axillary clearance in clinically node-positive disease and SN followed by axillary clearance in clinical node-negative disease where the sentinel node contains metastasis or is not found."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc45) <- "nkbcind"
