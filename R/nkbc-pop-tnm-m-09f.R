#' @export
nkbc09f <- list(
  code = "nkbc09f",
  kortnamn = "nkbc_pop_tnm_m_09f",
  lab = c(
    sv = "Fjärrmetastaser vid diagnos",
    en = "Distant metastases at diagnosis"
  ),
  pop = c(
    sv = "alla anmälda fall",
    en = "all reported cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x
      # Endast invasiv cancer
      # invasiv == "Invasiv cancer", Bortselekterat pga om väljer enbart invasiv
      # cancer så tas alla med uppgift saknas på invasiv bort. Dock några fel? reg
      # in situ och M1 men men...
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_mstad,
      outcome_en = d_mstad_en
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = c("a_pat_alder", "d_screening"),
  om_indikatorn = list(
    sv = "Fjärrmetastas vid diagnos definieras som fjärrmetastaserande sjukdom diagnosticerad inom 3 månader från bröstcancerdiagnos (cytdatum).",
    en = "Distant metastasis at diagnosis is defined as distant metastatic disease diagnosed within 3 months of breast cancer diagnosis (cytology/biopsy date)."
  ),
  vid_tolkning = list(
    sv = paste(
      "T.o.m. 2012 var det möjligt att registrera en tumör som att fjärrmetastaser ej kan bedömas (MX) i NKBC.",
      "Dessa har grupperats ihop med Uppgift saknas."
    ),
    en = NULL
  ),
  teknisk_beskrivning = NULL
)
class(nkbc09f) <- "nkbcind"
