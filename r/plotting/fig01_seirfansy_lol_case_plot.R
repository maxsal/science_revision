# libraries ----------
ally::libri(tidyverse, lubridate, ggsci, ggrepel,
            janitor, glue, here, ggtext, patchwork)
source(here("src", "extract_cfr.R"))

# prod <- TRUE
end_date <- "2021-06-30"

# load data ----------
obs <- fread("data/covid19india_national_counts_20211031.csv")[, date := as.Date(date)][date >= "2021-02-15"][, value := predict(loess(daily_cases ~ as.numeric(date), span = 0.25))][, .(date, value, variable = "Observed")]

pred <- fread("seirfansy_data/Daily15april.csv")[, date := as.Date(dates)]
pred <- melt(pred[, !c("V1", "dates")], id.vars = "date")
pred <- pred[
  , variable := fcase(
    variable == "P_daily_4", "4 weeks",
    variable == "P_daily_6", "6 weeks",
    variable == "P_daily_8", "8 weeks"
  )
][date >= as.Date("2021-04-15")][]

plot_data <- rbindlist(list(obs, pred), use.names = TRUE)[date <= end_date]

tmp_outname  <- "fig01_seirfansy_lol_case_plot.pdf"
tmp_title    <- "Effect of length of lockdown"

# plot -----------
colores <- c(
  "Observed" = "black",
  "4 weeks"  = pal_lancet()(3)[1],
  "6 weeks"  = pal_lancet()(3)[2],
  "8 weeks"  = pal_lancet()(3)[3]
)

cases_p <- plot_data %>% 
  filter(date >= "2021-02-15" & date <= end_date) %>%
  ggplot(aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = colores) +
  geom_vline(data = plot_data %>% 
               group_by(variable) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(variable, date) %>% 
               filter(!(variable %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = variable), 
             linetype = 'dashed') + 
  geom_label_repel(data = plot_data %>% 
                     group_by(variable) %>% 
                     filter(value == max(value)) %>% 
                     dplyr::ungroup() %>% 
                     select(variable, date, value) %>% 
                     filter(!(variable %in% c("Observed", "No intervention"))), 
                    aes(x = date, 
                        y = value, 
                        label = paste0(formatC(round(value), format="f", big.mark=",", digits=0), " cases"),
                        color = variable,
                        family = "Helvetica Neue"), 
                   nudge_y = 100000, 
                   nudge_x = -10, 
                   size = 4, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = tmp_title,
       y        = "Daily cases",
       x        = "",
       subtitle = glue::glue("February 15, 2021 to {format(as.Date(end_date), '%B %e, %Y')}"),
       color    = "Date of intervention") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B") +
  theme_classic() +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 11, vjust = 0.5),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 11, face = "bold"),
    axis.title.y    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = cases_p,
       height   = 5,
       width    = 7,
       units = "in", device = cairo_pdf)
