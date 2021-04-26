#' @export
nkbc08 <- list(
  code = "nkbc08",
  kortnamn = "nkbc_kir_omop_08",
  lab = c(
    sv = "Enbart en operation (ingen omoperation p.g.a. tumördata) i bröst",
    en = "Only one breast operation (no reoperation due to tumour data) "
  ),
  lab_short = c(
    sv = "Enbart en operation",
    en = "Only one breast operation"
  ),
  pop = c(
    sv = "opererade fall utan fjärrmetastaser vid diagnos",
    en = "operated cases without distant metastasis at diagnosis"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Opererade fall
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = ifelse(op_kir_sekbrost_Varde %in% c(0, 1), op_kir_sekbrost_Varde, NA),
      outcome = as.logical(!outcome)
    )
  },
  target_values = c(80, 90),
  period_dat_var = "op_kir_dat",
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = list(
    sv = paste(
      "Om analys av den bortopererade vävnaden visar att tumörvävnad kan ha kvarlämnats rekommenderas som regel en omoperation.",
      "Andelen bröstbevarande kirurgi påverkar risken för reoperation (vid högre andel primäroperation med mastektomi är risken för reoperation av naturliga skäl lägre)."
    ),
    en = paste(
      "If the microscopical analysis indicates that tumour tissue may have been left behind, reoperation is usually recommended.",
      "The proportion of breast-conserving surgery affects the risk of reoperation (with a higher proportion of primary surgery with mastectomy the risk of reoperation is lower)."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc08) <- "nkbcind"
