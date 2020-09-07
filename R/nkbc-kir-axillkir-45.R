#' @export
nkbc45 <- list(
  code = "nkbc45",
  kortnamn = "nkbc_kir_axillkir_45",
  lab = "Typ av axillkirurgi",
  lab_short = "Axillkirurgi",
  pop = "opererade fall med axillingrepp och utan fjärrmetastaser vid diagnos",
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
      )
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv", "d_nstad"),
  other_vars_inca = c("a_pat_alder", "d_invasiv", "d_nstad", "d_prim_beh"),
  om_indikatorn = c(
    "Axillkirurgi utförs för att klarlägga om spridning finns till armhålans lymfkörtlar.",
    "Förenklat utförs sentinel node biopsi vid kliniskt nodnegativ sjukdom, axillutrymning vid kliniskt nodpositiv sjukdom och SN och utrymning vid kliniskt nodnegativ sjukdom där sentinel node inte hittas eller innehåller metastas."
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc45) <- "nkbcind"
