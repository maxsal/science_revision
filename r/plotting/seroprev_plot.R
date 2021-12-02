
sero_data <- data.table(
  survey         = c("first", "second", "third", "fourth"),
  survey_date    = as.Date(c("2020-06-15", "2020-09-15", "2021-01-15", "2021-07-15")),
  seroprevalence = c(0.0073, 0.071, .241, .676),
  source         = c("Murhekar et al. 2020", "Murhekar et al. 2021a", "Murhekar et al. 2021b", "Sharma"),
  source_doi     = c("10.4103/ijmr.IJMR_3290_20", "10.1016/S2214-109X(20)30544-1", "10.1016/j.ijid.2021.05.040", "https://indianexpress.com/article/explained/explained-icmr-covid-fourth-serosurvey-findings-7413949/"),
  type           = "Seroprevalence"
)

vax_data <- fread("data/covid19india_vaccine_data_20211031.csv")[place == "India"][date <= sero_data[, max(survey_date)]]

combined_data <- rbindlist(
  list(
    rbindlist(list(
      sero_data,
      vax_data[seq(1, nrow(vax_data), 10)][
        , .(survey_date = date, pct_one_dose)][
          , survey_date := as.Date(survey_date)][
            , type := "At least 1 dose"][]), fill = TRUE),
    vax_data[seq(1, nrow(vax_data), 10)][
      , .(survey_date = date, pct_two_doses)][
        , survey_date := as.Date(survey_date)][
          , type := "Two doses"][]), fill = TRUE)[
              type == "Seroprevalence", estimate := seroprevalence][
                type == "At least 1 dose", estimate := pct_one_dose / 100][
                  type == "Two doses", estimate := pct_two_doses / 100][
                  , type := factor(type, levels = c("Seroprevalence", "At least 1 dose", "Two doses"))]
    
colors <- c(ggsci::pal_lancet()(3)[2], ggsci::pal_lancet()(3)[3], ggsci::pal_lancet()(3)[1])

(seroprevalence_plot <- combined_data |>
  ggplot(aes(x = survey_date, y = estimate, color = type)) +
  geom_vline(xintercept = as.Date("2021-01-16"), linetype = 2, color = "gray40") +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "COVID-19 seroprevalence over time",
    x     = "Date",
    y     = "COVID-19 seroprevalence",
    caption = "**Notes:** Vertical dashed line represents vaccination start date (January 16, 2021)"
  ) +
  theme(
    text = element_text(family = "Helvetica Neue")
  ))

ggsave(filename = "fig/response/seroprev_plot.pdf", plot = seroprevalence_plot,
       width = 6, height = 4, device = cairo_pdf)
ggsave(filename = "fig/response/seroprev_plot.png", plot = seroprevalence_plot,
       width = 6, height = 4, units = "in", dpi = 320, device = png)
