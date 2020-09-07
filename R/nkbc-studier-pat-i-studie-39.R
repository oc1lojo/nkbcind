#' @export
nkbc39 <- list(
  code = "nkbc39",
  kortnamn = "nkbc_studier_pat_i_studie_39",
  lab = "Patienten ingår i studie",
  pop = "opererade fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      a_beh_studie = as.logical(ifelse(a_beh_studie_Varde %in% c(0, 1), a_beh_studie_Varde, NA)),
      pre_beh_studie = as.logical(ifelse(pre_beh_studie_Varde %in% c(0, 1), pre_beh_studie_Varde, NA)),
      post_beh_studie = as.logical(ifelse(post_beh_studie_Varde %in% c(0, 1), post_beh_studie_Varde, NA)),
      # Beräkna indikator
      outcome =
        dplyr::case_when(
          a_beh_studie | pre_beh_studie | post_beh_studie ~ TRUE,
          !a_beh_studie | !pre_beh_studie | !post_beh_studie ~ FALSE
        )
    )
  },
  sjhkod_var = "post_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  other_vars_inca = c("a_pat_alder", "d_invasiv", "d_vitalstatus"),
  om_indikatorn =
    paste(
      "Ett övergripande mål är att erbjuda alla bröstcancerpatienter medverkan i studier för att utveckla nya behandlingar och arbetssätt.",
      "Indikatorn gäller alla typer av studier (t.ex. kliniska studier, omvårdnadsstudier, fysioterapi-studier).",
      "Indikatorn infördes 2017 och bör tolkas med försiktighet p.g.a. underrapportering och regionala skillnader i definition."
    ),
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc39) <- "nkbcind"
