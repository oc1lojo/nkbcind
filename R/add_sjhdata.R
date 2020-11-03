#' @export
add_sjhdata <- function(x,
                        sjukhuskoder = sjukhuskoder,
                        sjhkod_var = "a_inr_sjhkod",
                        samredovisning_skaraborg = TRUE,
                        samredovisning_lund_malmo = TRUE,
                        samredovisning_lund_malmo_onkbeh = TRUE,
                        ...) {
  if (!samredovisning_lund_malmo_onkbeh & samredovisning_lund_malmo) {
    stop("Arguments samredovisning_lund_malmo_onkbeh and samredovisning_lund_malmo are not consistent")
  }

  names(x)[names(x) == sjhkod_var] <- "sjhkod"

  x <- x %>%
    dplyr::mutate(sjhkod = as.integer(sjhkod)) %>%
    dplyr::left_join(sjukhuskoder, by = c("sjhkod" = "sjukhuskod")) %>%
    dplyr::mutate(
      region = dplyr::case_when(
        region_sjh_txt == "Sthlm/Gotland" ~ 1L,
        region_sjh_txt == "Uppsala/Örebro" ~ 2L,
        region_sjh_txt == "Sydöstra" ~ 3L,
        region_sjh_txt == "Syd" ~ 4L,
        region_sjh_txt == "Väst" ~ 5L,
        region_sjh_txt == "Norr" ~ 6L,
        TRUE ~ NA_integer_
      ),
      region = dplyr::if_else(is.na(region), d_region_lkf, region),
      landsting = substr(sjhkod, 1, 2) %>% as.integer(),
      # Fulfix Bröstmottagningen, Christinakliniken Sh & Stockholms bröstklinik så hamnar i Stockholm
      landsting = dplyr::if_else(
        sjhkod %in% c(97333, 97563), 10L, landsting
      ),
      landsting = dplyr::if_else(
        landsting %in% c(
          seq(10, 13),
          seq(21, 28),
          30,
          seq(41, 42),
          seq(50, 57),
          seq(61, 65)
          # seq(91,96)
        ),
        landsting,
        NA_integer_
      )
    )

  # Ev. samredovisning av sjukhus
  x <- x %>%
    dplyr::mutate(
      sjukhus = dplyr::case_when(
        samredovisning_skaraborg &
          sjukhus %in% c("Lidköping", "Skövde") ~ "Skaraborg",
        samredovisning_lund_malmo &
          sjukhus %in% c("Lund", "Malmö") ~ "Lund/Malmö",
        samredovisning_lund_malmo_onkbeh &
          sjhkod_var %in% c(
            "post_inr_sjhkod", "pre_inr_sjhkod",
            "d_onk_sjhkod", "d_onkpreans_sjhkod", "d_onkpostans_sjhkod",
            "d_prim_beh_sjhkod"
          ) &
          sjukhus %in% c("Lund", "Malmö") ~ "Lund/Malmö",
        TRUE ~ sjukhus
      )
    )

  return(x)
}
