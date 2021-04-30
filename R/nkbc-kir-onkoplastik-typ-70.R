#' @export
nkbc70 <- list(
  code = "nkbc70",
  kortnamn = "nkbc_kir_onkoplastik_typ_70",
  lab = c(
    sv = "Typ av onkoplastikkirurgi"
  ),
  lab_short_w_pop = c(
    sv = "Typ av onkoplastikkirurgi vid partiell mastektomi"
  ),
  pop = c(
    sv = "fall opererade med partiell mastektomi och onkoplastikkirurgi utan fjärrmetastaser vid diagnos"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Uppgifter började registreras våren 2020
      lubridate::year(a_diag_dat) >= 2020,

      # Opererade med direktrekonstruktion/onkoplastikkirurgi
      !is.na(dir_kir_dat),

      # Partiell mastektomi
      dir_kir_brost_Varde %in% 1,

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = factor(
        dplyr::if_else(dir_kir_onkoplasttyp_Varde %in% c(2, 4), 2L, dir_kir_onkoplasttyp_Varde),
        levels = c(1, 2, 3),
        labels = c(
          "Omformning (intern lambå)\n+ sutur (HAB40+ZZR70)",
          "Reduktionsplastik\n(HAB40+HAD30/35)",
          "Volymssubstitution med\nlambå (HAB40+HAE10)"
        )
      )
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = NULL,
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc70) <- "nkbcind"
