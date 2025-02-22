library(ncpublicbenefits)


base_table <- base_composition()

child_care <- child_care(base_table)
fns <- fns_snap(base_table)

all_benefits <- create_benefits_table(100)

library(ncpublicbenefits)
library(ggplot2)

# Create a comprehensive benefits table with income increments of 100
benefits_table <- create_benefits_table(10)

unique_composition <- family_benefit_values('family')[3]
unique_benefits <- family_benefit_values('benefits')[1:3]

benefits_single_family_type <- benefits_table |>
    filter(composition == !!unique_composition)

ggplot(benefits_single_family_type, aes(x = monthly_income, y = payment, color = benefit)) +
    geom_line() +
    labs(
      title = "Benefit levels by income",
      subtitle = paste0("Family type: ", unique_composition),
      x = "Monthly Income",
      y = "Benefit Amount",
      lab = "Benefit"
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_minimal() +
    theme(legend.position = "bottom")

income_plus_benefits <- benefits_table |>
    total_take_home_income(unique_composition, unique_benefits) |>
    select(.data$monthly_income, .data$income_minus_taxes, .data$take_home) |>
    pivot_longer(cols = c('income_minus_taxes', 'take_home'), names_to = 'pay_type', values_to = 'income') |>
    mutate(
      pay_type = case_match(
        .data$pay_type,
        'income_minus_taxes' ~ 'Income minus taxes',
        'take_home' ~ 'Income plus benefits, minus taxes'
      )
    ) |>
    arrange(.data$pay_type, .data$monthly_income)

ggplot(income_plus_benefits, aes(x = monthly_income, y = income, color = pay_type)) +
  geom_line() +
  labs(
    title = "Total take home income by wage",
    subtitle = paste0("Family type: ", unique_composition),
    x = "Monthly wage income",
    y = "Total take home income",
    caption = paste0("Benefits:\n", paste(unique_benefits, collapse = ", ")),
    labs = NULL
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()  +
  theme(
    plot.caption = element_text(hjust = 0),
    legend.position = "bottom"
  )

