#' @export
nkbc09g <- list(
  code = "nkbc09g",
  kortnamn = "nkbc_pop_n_09g",
  lab = "Spridning till lymfkörtlarna vid operation",
  pop = "opererade fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Enbart opererade
      !is.na(op_kir_dat),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_pnstat
    )
  },
  sjhkod_var = "op_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_invasiv"),
  om_indikatorn =
    paste(
      "Kännedom om tumörspridning till armhålans lymfkörtlar ger prognostisk information inför val av postoperativ onkologisk behandling.",
      "Tumörspridning fastställs genom mikroskopisk analys av sentinel node och/eller övriga lymfkörtlar i armhålan."
    ),
  vid_tolkning =
    c(
      "I populationen ingår både primärt opererade och opererade efter påbörjad/genomförd preoperativ onkologisk behandling.",
      "Spridning till lymfkörtlar i armhålan definieras som metastas > 0.2 mm."
    ),
  teknisk_beskrivning = NULL
)
class(nkbc09g) <- "nkbcind"
