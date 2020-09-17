#' @export
nkbc09d <- list(
  code = "nkbc09d",
  kortnamn = "nkbc_pop_subtyp_09d",
  lab = c(
    sv = "Biologisk subtyp vid diagnos",
    en = "Biological subtype at diagnosis"
  ),
  lab_short = c(
    sv = "Biologisk subtyp",
    en = "Biological subtype"
  ),
  pop = c(
    sv = "alla anmälda invasiva fall",
    en = "all reported invasive cases"
  ),
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer"
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = d_trigrp,
      outcome_en = d_trigrp_en
    )
  },
  sjhkod_var = "a_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = list(
    sv = c(
      paste(
        "Invasiv bröstcancer kan delas in i subtyper utefter tumörens biologiska egenskaper.",
        "I klinisk vardag baseras indelningen på analys av biomarkörerna ER, PgR, Ki67 och HER2 i tumörvävnaden.",
        "Olika subtyper är känsliga för olika typer av behandlingar."
      ),
      "Luminala – tumörer som uttrycker östrogenreceptorer (ER-positiva) och/eller progesteronreceptorer (PgR-positiva), men saknar amplifiering av HER2-genen (HER2-negativa).",
      "HER2-positiva – tumörer med ett överuttryck av tillväxtfaktorreceptorn HER2 eller ett ökat antal kopior av HER2-genen (amplifiering).",
      "Trippelnegativa – tumörer som saknar receptorer för östrogen (ER), progesteron (PgR) och HER2."
    ),
    en = c(
      paste(
        "Breast cancer subtypes are defined according to the biologic properties of the invasive cancer.",
        "Different subtypes respond to different therapies."
      ),
      "Luminal – tumours that express oestrogen (ER) and/or progesterone receptors (PgR) i.e. are ER and/or PgR positive, but lack amplification of the HER2-gene.",
      "HER2-positive – tumours with high expression of the HER2-receptor or multiple copies of the HER2 gene (amplification).",
      "Triple negative – tumours lacking oestrogen receptors (ER), progesterone receptors (PgR) and HER2 amplification."
    )
  ),
  vid_tolkning = NULL,
  teknisk_beskrivning = NULL
)
class(nkbc09d) <- "nkbcind"
