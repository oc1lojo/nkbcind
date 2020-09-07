#' @export
nkbc33 <- list(
  code = "nkbc33",
  kortnamn = "nkbc_tackning_mot_cancerreg_33",
  lab = "Täckningsgrad mot cancerregistret",
  pop = "alla anmälda fall till cancerregistret",
  pop_short = "alla anmälda fall",
  target_values = c(95, 99),
  sjhkod_var = "a_inr_sjhkod",
  om_indikatorn =
    paste(
      "Alla vårdgivare inom hälso- och sjukvård är skyldiga att rapportera anmälningspliktiga tumörer till cancerregistret.",
      "Anmälan skall ske både av ansvarig kliniker och av den patolog som fastställt diagnosen.",
      "För att undvika dubbelarbete innebär en anmälan till kvalitetsregistret automatiskt också en canceranmälan."
    ),
  vid_tolkning = "Cancerregistret omfattar även personer med skyddad identitet och/eller dem som avböjt vara med i kvalitetsregistret.",
  teknisk_beskrivning = NULL
)
class(nkbc33) <- c("nkbc33", "nkbcind")
