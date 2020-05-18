# Jfr https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny/src/2017.1/nkbc25.R

nkbc25 <- list(
  code = "nkbc25",
  kortnamn = "nkbc_studier_pat_i_postop_studie_25",
  lab = "Patienten ingår i postoperativ studie",
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
      outcome = as.logical(ifelse(post_beh_studie_Varde %in% c(0, 1), post_beh_studie_Varde, NA))
    )
  },
  sjhkod_var = "post_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv", "d_vitalstatus"),
  om_indikatorn =
    paste(
      "Ett övergripande mål är att erbjuda alla bröstcancerpatienter medverkan i studier för att utveckla nya behandlingar och arbetssätt.",
      "Indikatorn gäller alla typer av studier (t.ex. kliniska studier, omvårdnadsstudier, fysioterapi-studier)."
    ),
  vid_tolkning = NULL,
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc25) <- "nkbcind"
