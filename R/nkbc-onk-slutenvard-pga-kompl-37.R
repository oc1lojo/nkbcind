# Jfr https://bitbucket.org/cancercentrum/nkbc-onlinerapporter/src/2018-05-19/nkbc37.R#lines-194

#' @export
nkbc37 <- list(
  code = "nkbc37",
  kortnamn = "nkbc_onk_slutenvard_pga_kompl_37",
  lab = c(
    sv = "Slutenvård p.g.a. behandlingskomplikationer av pre- och/eller postoperativ cytostatikabehandling"
  ),
  lab_short = c(
    sv = "Slutenvård p.g.a. behandlingskomplikationer av cytostatikabehandling"
  ),
  pop = c(
    sv = "opererade, cytostatikabehandlade fall utan fjärrmetastaser vid diagnos"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast cytostatikabehandlade
      d_kemo == TRUE,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Går på det värde som finns
      pre_kemo_kompl_Varde = ifelse(pre_kemo_kompl_Varde %in% c(0, 1), pre_kemo_kompl_Varde, NA),
      post_kemo_kompl_Varde = ifelse(post_kemo_kompl_Varde %in% c(0, 1), post_kemo_kompl_Varde, NA),
      d_kemo_kompl_Varde = pmax(post_kemo_kompl_Varde, pre_kemo_kompl_Varde, na.rm = TRUE),

      # Hantera missing
      outcome = as.logical(ifelse(d_kemo_kompl_Varde %in% c(0, 1), d_kemo_kompl_Varde, NA))
    )
  },
  sjhkod_var = "d_onk_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc37) <- "nkbcind"
