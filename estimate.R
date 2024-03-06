library(data.table)
library(readxl)
library(janitor)
library(naniar)
library(writexl)
library(forecast)
library(dpm)
library(plm)
library(gausscov)
library(ggplot2)


# import data
excel_file = "data/Kopija datoteke Tablica podataka.xlsx"
excel_sheets(excel_file)
dependent = read_excel(excel_file, sheet = "Adjusted net savings-zavis.vari", n_max = 15)
dependent = as.data.table(dependent)
predictors = read_excel(excel_file, sheet = "Pomoćne varijable do 2021")
predictors = as.data.table(predictors)

# Clean table for dependent variable
cols = c("Country Name", colnames(dependent)[5:ncol(dependent)])
dependent = dependent[, ..cols]
dependent = melt(
  dependent,
  id.vars = "Country Name",
  variable.name = "year",
  value.name = "adj_svng_gn"
)
dependent[, adj_svng_gn := as.numeric(adj_svng_gn)]
dependent[, year := as.integer(substr(year, 1, 4))]
setnames(dependent, "Country Name", "country")
dependent = dependent[year < 2021]

# Clean table for dependent variable
setnames(predictors, c("country", "country_id", "year", "domestic_credit_to_private",
                       "rule_of_law", "gdp_pc", "fdi_stock_tuorism", "fdi_stock_manuf",
                       "adj_savings", "fdi_manufacturing_gdp", "fdi_tourism_gdp",
                       "trade_index", "control_of_corruption",
                       "fdi_inward_stock_BDP"))

# Merge data
dt = merge(dependent, predictors, by = c("country", "year"))

# Remove country_id
dt[, country_id := NULL]

# Set all columns to numeric
cols = colnames(dt)[-c(1:2)]
dt = dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

# Log positive variables
only_posistive = dt[, lapply(.SD, function(x) all(x > 0, na.rm = TRUE)),
                    .SDcols = -c("country", "year")]
only_posistive = unlist(only_posistive)
only_posistive = names(only_posistive[only_posistive == TRUE])
dt[, (only_posistive) := lapply(.SD, function(x) log(x)), .SDcols = only_posistive]

# Missing values
vis_miss(dt)
gg_miss_var(dt)
gg_miss_var(dt, facet = year)
gg_miss_var(dt, facet = country)

# Select variables
cols = setdiff(colnames(dt), c("domestic_credit_to_private", "control_of_corruption"))
dt = dt[, .SD, .SDcols = cols]

# Locf missing values
dt = dt[, nafill(.SD, type = "locf"), by = country]

# Remove Estonia because of missing values
dt = dt[country %notin% c("Estonia", "Romania")]

# Visualize variables
dt[, .(country, year, adj_svng_gn)] |>
  ggplot(aes(year, adj_svng_gn)) +
  geom_line() +
  facet_wrap(~country)

# check for stationarity
num_diffs = dt[, lapply(.SD, ndiffs), .SDcols = -c("country", "year"), by = country]
num_diffs = num_diffs[, colSums(.SD) / nrow(num_diffs) > 0.5, .SDcols = -c("country")]
num_diffs = num_diffs[num_diffs == TRUE]
dt[, (names(num_diffs)) := lapply(.SD, function(x) c(NA, diff(x))),
   .SDcols = names(num_diffs), by = country]

# Visualize variables
dt[, .(country, year, adj_svng_gn)] |>
  ggplot(aes(year, adj_svng_gn)) +
  geom_line() +
  facet_wrap(~country)

# Measure opf balances
punbalancedness(dt)

# Missing values
vis_miss(dt)
gg_miss_var(dt)
gg_miss_var(dt, facet = year)
gg_miss_var(dt, facet = country)

# Keep data after 1998
dt = dt[year > 1998]

# Remove missing values for dependent variable
dt = na.omit(dt, cols = "adj_svng_gn")

# fixed and random effects model
dt_ = dt[, .(country, year, adj_svng_gn, rule_of_law, gdp_pc, trade_index,
             fdi_inward_stock_BDP, fdi_manufacturing_gdp, fdi_tourism_gdp)]
X_names = paste0(colnames(dt_)[-c(1:3)], collapse = " + ")
formula_ = as.formula(paste0("adj_svng_gn ~ ", X_names))
fe_model = plm(
  formula_,
  data = dt_,
  index = c("country", "year"),
  model = "within",
  effect = "twoways"
)
summary(fe_model)
re_model = plm(
  formula_,
  data = dt,
  index = c("country", "year"),
  model = "random",
  effect = "twoways"
)
summary(re_model)

# Haussman test
phtest(fe_model, re_model)
phtest(formula_, data = dt, method = "aux", vcov = vcovHC)

# Make Breusch-Pagan test
pcdtest(re_model, test = "lm")

# Check significant variables with gausscov
dt_ = copy(dt)
dt_[, nafill(.SD, type = "locf"), by = country]
dt_ = dt_[, 3:ncol(dt)]
dt_ = na.omit(dt_)
dt_ = as.matrix(dt_)
f1st(dt_)

# Estimate all combinations
vars = colnames(dt)[3:ncol(dt)]
formula_prefix = "adj_svng_gn ~ gdp_pc + trade_index"
combinations = combn(vars, 2)
dim(combinations)
