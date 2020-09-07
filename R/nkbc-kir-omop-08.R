nkbc08 <- list(
  code = "nkbc08",
  kortnamn = "nkbc_kir_omop_08",
  lab = "Enbart en operation (ingen omoperation p.g.a. tumördata) i bröst",
  lab_short = "Enbart en operation",
  pop = "opererade fall utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
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
  om_indikatorn =
    paste(
      "Om analys av den bortopererade vävnaden visar sig att tumörvävnad kan ha kvarlämnats rekommenderas som regel en omoperation.",
      "Andelen bröstbevarande kirurgi påverkar risken för reoperation (vid högre andel primäroperation med mastektomi är risken för reoperation av naturliga skäl lägre)."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc08) <- "nkbcind"
