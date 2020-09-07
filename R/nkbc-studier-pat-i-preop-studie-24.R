# Jfr https://bitbucket.org/cancercentrum/nkbc-arsrapportshiny/src/2017.1/nkbc24.R

nkbc24 <- list(
  code = "nkbc24",
  kortnamn = "nkbc_studier_pat_i_preop_studie_24",
  lab = "Patienten ingår i preoperativ studie",
  pop = "fall utan fjärrmetastaser vid diagnos med preoperativ onkologisk behandling",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast preop onk behandling (planerad om utförd ej finns). Egentligen onödigt, bör inte finnas andra.
      d_prim_beh_Varde == 2,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      # Hantera missing
      outcome = as.logical(ifelse(pre_beh_studie_Varde %in% c(0, 1), pre_beh_studie_Varde, NA))
    )
  },
  sjhkod_var = "pre_inr_sjhkod",
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
class(nkbc24) <- "nkbcind"
