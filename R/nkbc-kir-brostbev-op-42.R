nkbc42 <- list(
  code = "nkbc42",
  kortnamn = "nkbc_kir_brostbev_op_42",
  lab = "Bröstbevarande operation",
  pop = "opererade fall med bröstkirurgi (fall med enbart axillkirurgi exkluderade) utan fjärrmetastas vid diagnos",
  pop_short = "opererade fall med bröstkirurgi utan fjärrmetastas vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast opererade
      !is.na(op_kir_dat),

      # Exkludera enbart axillkirurgi eller missing
      op_kir_brost_Varde %in% c(1, 2, 4),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = dplyr::case_when(
        op_kir_brost_Varde %in% 1 ~ TRUE,
        op_kir_brost_Varde %in% c(2, 4) ~ FALSE,
        TRUE ~ NA
      )
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  other_vars_inca = c("a_pat_alder", "d_invasiv", "d_prim_beh"),
  om_indikatorn =
    paste(
      "Bröstbevarande ingrepp i kombination med strålbehandling är standardingreppet för majoriten av tidigt upptäckta bröstcancrar.",
      "Bl.a. tumörens storlek och lokalisation liksom bröstet storlek spelar roll för av av kirurgisk operationsmetod."
    ),
  vid_tolkning = "Case-mix liksom lokala terapitraditioner, exempelvis andelen preoperativt onkologiskt behandlade, påverkar andelen bröstbevarande kirurgi.",
  teknisk_beskrivning = NULL
)
class(nkbc42) <- "nkbcind"
