nkbc27 <- list(
  code = "nkbc27",
  kortnamn = "nkbc_onk_postop_cytostatikabeh_bland_erneg_27",
  lab = "Postoperativ cytostatikabehandling",
  pop = "primärt opererade östrogenreceptornegativa invasiva fall med tumörstorlek >10 mm eller spridning till lymfkörtlar utan fjärrmetastaser vid diagnos",
  pop_short = "primärt opererade ER- invasiva fall med större tumörer eller spridning till lymfkörtlar utan fjärrmetastaser vid diagnos",
  filter_pop = function(x, ...) {
    dplyr::filter(
      x,
      # Reg av given onkologisk behandling
      lubridate::year(a_diag_dat) >= 2012,

      # Endast opererade
      !is.na(op_kir_dat),

      # Endast primär opereration (planerad om utfärd ej finns)
      # (pga att info om tumörstorlek och spridning till N behövs)
      d_prim_beh_Varde == 1,

      # Endast invasiv cancer
      d_invasiv == "Invasiv cancer",

      # ER-
      d_er_Varde == 2,

      # Tumörstorlek > 10 mm eller spridning till lymfkörtlar
      (op_pad_invstl > 10 | op_pad_lglmetant > 0),

      # Ej fjärrmetastaser vid diagnos
      !a_tnm_mklass_Varde %in% 10
    )
  },
  mutate_outcome = function(x, ...) {
    dplyr::mutate(x,
      outcome = as.logical(post_kemo_Varde)
    )
  },
  target_values = c(80, 90),
  sjhkod_var = "post_inr_sjhkod",
  other_vars = "a_pat_alder",
  om_indikatorn = "Cytostatikabehandling rekommenderas i allmänhet vid bröstcancer med spridning till axillens lymfkörtlar, men även om tumören har svag hormonell känslighet och/eller då det föreligger riskfaktorer.",
  vid_tolkning =
    c(
      "Enbart postoperativ cytostatikabehandling är medtaget i beräkningen, vilket innebär att andelen är mindre för de sjukhus där cytostatika i högre utsträckning ges preoperativt.",
      "Tumörstorlek är definierat som storlek på den största invasiva tumören, vilket innebär att vissa multifokala fall med total utbredning >10 mm inte finns med i urvalet.",
      "Spridning till lymfkörtlar i armhålan definieras som metastas >0.2 mm."
    ),
  inkl_beskr_onk_beh = TRUE,
  teknisk_beskrivning = NULL
)
class(nkbc27) <- "nkbcind"
