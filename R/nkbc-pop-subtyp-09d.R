#' @export
nkbc09d <- list(
  code = "nkbc09d",
  kortnamn = "nkbc_pop_subtyp_09d",
  lab = "Biologisk subtyp vid diagnos",
  lab_short = "Biologisk subtyp",
  pop = "alla anmälda invasiva fall",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer"
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_trigrp
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn =
    c(
      paste(
        "Invasiv bröstcancer kan delas in i subtyper utefter tumörens biologiska egenskaper.",
        "I klinisk vardag baseras indelningen på analys av biomarkörerna ER, PgR, Ki67 och HER2 i tumörvävnaden.",
        "Olika subtyper är känsliga för olika typer av behandlingar."
      ),
      "Luminala – tumörer som uttrycker östrogenreceptorer (ER-positiva) och/eller progesteronreceptorer (PgR-positiva), men saknar amplifiering av HER2-genen (HER2-negativa).",
      "HER2-positiva – tumörer med ett överuttryck av tillväxtfaktorreceptorn HER2 eller ett ökat antal kopior av HER2-genen (amplifiering).",
      "Trippelnegativa – tumörer som saknar receptorer för östrogen (ER), progesteron (PgR) och HER2."
    ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09d) <- "nkbcind"
