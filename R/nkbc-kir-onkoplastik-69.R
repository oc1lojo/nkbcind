#' @export
nkbc69 <- list(
  code = "nkbc69",
  kortnamn = "nkbc_kir_onkoplastik_69",
  lab = c(
    sv = "Onkoplastikkirurgi"
  ),
  lab_short_w_pop = c(
    sv = "Onkoplastikkirurgi vid partiell mastektomi"
  ),
  pop = c(
    sv = "fall opererade med partiell mastektomi utan fjärrmetastaser vid diagnos"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Uppgifter började registreras hösten 2017
      lubridate::year(a_diag_dat) >= 2018,

      # Enbart opererade
      !is.na(op_kir_dat),

      # Partiell mastektomi
      op_kir_brost_Varde %in% 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(op_kir_onkoplastik_Varde %in% c(0, 1), op_kir_onkoplastik_Varde, NA))
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc69) <- "nkbcind"
