#' @export
nkbc33 <- list(
  code = "nkbc33",
  kortnamn = "nkbc_tackning_mot_cancerreg_33",
  lab = c(
    sv = "Täckningsgrad mot cancerregistret",
    en = "Coverage compared to the Swedish Cancer Register"
  ),
  pop = c(
    sv = "alla anmälda fall till cancerregistret",
    en = "all reported cases to the Swedish Cancer Register"
  ),
  target_values = c(95, 99),
  sjhkod_var = "a_inr_sjhkod",
  om_indikatorn = list(
    sv = paste(
      "Alla vårdgivare inom hälso- och sjukvård är skyldiga att rapportera anmälningspliktiga tumörer till cancerregistret.",
      "Anmälan skall ske både av ansvarig kliniker och av den patolog som fastställt diagnosen.",
      "För att undvika dubbelarbete innebär en anmälan till kvalitetsregistret automatiskt också en canceranmälan."
    ),
    en = paste(
      "All Swedish healthcare providers are obliged to report notifiable tumours to the Swedish Cancer Register.",
      "The report must be made both by the responsible clinician and by the pathologist who established the diagnosis.",
      "To avoid duplication of work, a notification to the NKBC automatically also means a Cancer Register notification."
    )
  ),
  vid_tolkning = list(
    sv = "Cancerregistret omfattar även personer med skyddad identitet och/eller dem som avböjt vara med i kvalitetsregistret.",
    en = "The Swedish Cancer Register also includes persons with a protected identity and/or those who have declined to be included in NKBC."
  ),
  teknisk_beskrivning = NULL
)
class(nkbc33) <- c("nkbc33", "nkbcind")
