nkbc09f <- list(
  code = "nkbc09f",
  kortnamn = "nkbc_pop_tnm_m_09f",
  lab = "Fjärrmetastaser vid diagnos",
  pop = "alla anmälda fall",
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
      outcome = d_mstad
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = "Fjärrmetastas vid diagnos definieras som fjärrmetastaserande sjukdom diagnosticerad inom 3 månader från bröstcancerdiagnos (cytdatum).",
  vid_tolkning =
    paste(
      "T.o.m. 2012 var det möjligt att registrera en tumör som att fjärrmetastaser ej kan bedömas (MX) i NKBC.",
      "Dessa har grupperats ihop med Uppgift saknas."
    ),
  teknisk_beskrivning = NULL
)
class(nkbc09f) <- "nkbcind"
