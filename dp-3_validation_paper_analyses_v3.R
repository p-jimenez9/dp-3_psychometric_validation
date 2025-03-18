## 0) Script description:##############################################
#
# Date: 2024/09/26
# Author: Pol Jimenez-Arenas
# Topic: BiSC DP-3 psychometric validation paper
# Objective: Conduct the analyses of the paper on the psychometric
# validation of the DP-3 instrument used in BiSC, by using the data
# generated at 28 months
##

##
# version: v.3.0
# latest version date: 2025/03/01
##


## 1) Settings:########################################################

# To clean, to set working directory

rm(list=ls())

setwd("//FS05.isglobal.lan/HPC_BISC_DATA/analyses/BiSC_24/012_Pol_Jimenez/2_dp3_validation/")
dir()


## 2) Read db:#########################################################

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/postnatal/8m/neuro/dp_3/db/output/dp3_8m_clean_PJ_20250114.RData")
db8m <- db
rm(db)
db8m$id_bisc <- as.character(db8m$id_bisc)

load("//FS05.isglobal.lan/HPC_BISC_DATA/analyses/BiSC_24/012_Pol_Jimenez/2_dp3_validation/db/output/dp3_28m_clean_PJ_06082024.RData")
db28m <- db_clean
rm(db_clean)


## 3) Adapt db and add covariates:#####################################

db8m$scale <- NULL
db8m$respuestas <- NULL
db8m$dp3_tipus <- NULL
db8m$respondant <- as.character(db8m$respondant)
change <- which(startsWith(db8m$respondant, "1.8"))
db8m[change,"respondant"] <- "Seleccione el informador"
db8m$respondant <- as.factor(db8m$respondant)
change <- which(startsWith(db8m$id_bisc, "10"))
db8m[change,"hosp"] <- "10"
change <- which(startsWith(db8m$id_bisc, "11"))
db8m[change,"hosp"] <- "11"
change <- which(startsWith(db8m$id_bisc, "12"))
db8m[change,"hosp"] <- "12"
db8m$hosp <- as.factor(db8m$hosp)

db28m$Baremo <- NULL
db28m$Respuestas <- NULL
db28m$Aplicador <- as.factor(db28m$Aplicador)
change <- which(startsWith(as.character(db28m$id_bisc), "10"))
db28m[change,"hosp"] <- "10"
change <- which(startsWith(as.character(db28m$id_bisc), "11"))
db28m[change,"hosp"] <- "11"
change <- which(startsWith(as.character(db28m$id_bisc), "12"))
db28m[change,"hosp"] <- "12"
db28m$hosp <- as.factor(db28m$hosp)

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/prenatal/Clinapsis/data/mid/1_Midwife12w/Old/mid12_v12_2024_05_06_AM.RData")
mid12 <- mid12[, c("codigo_participante", "ethnicity_c", "parity_m_2cat", "educ_level_m_3cat")]
mid12$ethnicity_c_2cat <- "other"
change <- which(mid12$ethnicity_c == "eur")
mid12[change,"ethnicity_c_2cat"] <- "eur"
mid12$ethnicity_c <- NULL
mid12$ethnicity_c_2cat <- as.factor(mid12$ethnicity_c_2cat)
mid12$educ_level_m_2cat <- "sec_or_lower"
change <- which(mid12$educ_level_m_3cat == "University")
mid12[change,"educ_level_m_2cat"] <- "univ_or_higher"
mid12$educ_level_m_3cat <- NULL
mid12$educ_level_m_2cat <- as.factor(mid12$educ_level_m_2cat)

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/prenatal/Clinapsis/data/mid/16_Parto/Old/parto_v23_2024_06_26_AM.RData")
parto <- parto[, c("codigo_participante", "b_peso", "gestage_0y_c_weeks", "b_sexo")]
parto$b_sexo <- as.factor(parto$b_sexo)

db8m <- merge(db8m, mid12, by.x = "id_bisc", by.y = "codigo_participante", all.x = TRUE)
db28m <- merge(db28m, mid12, by.x = "id_bisc", by.y = "codigo_participante", all.x = TRUE)
db8m <- merge(db8m, parto, by.x = "id_bisc", by.y = "codigo_participante", all.x = TRUE)
db28m <- merge(db28m, parto, by.x = "id_bisc", by.y = "codigo_participante", all.x = TRUE)

rm(list = c("mid12", "parto"))

# to generate the baseline tab with pvals
fullsample <- db28m[, c("id_bisc", "hosp", "parity_m_2cat", "ethnicity_c_2cat", 
                        "educ_level_m_2cat", "b_peso", "gestage_0y_c_weeks", "b_sexo")]
fullsample$group <- "dp3"
fullsample$group <- as.factor(fullsample$group)

exclude_ids <- db28m$id_bisc

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/prenatal/Clinapsis/data/mid/1_Midwife12w/Old/mid12_v12_2024_05_06_AM.RData")
mid12 <- mid12[, c("codigo_participante", "ethnicity_c", "parity_m_2cat", "educ_level_m_3cat")]
mid12$ethnicity_c_2cat <- "other"
change <- which(mid12$ethnicity_c == "eur")
mid12[change,"ethnicity_c_2cat"] <- "eur"
mid12$ethnicity_c <- NULL
mid12$ethnicity_c_2cat <- as.factor(mid12$ethnicity_c_2cat)
mid12$educ_level_m_2cat <- "sec_or_lower"
change <- which(mid12$educ_level_m_3cat == "University")
mid12[change,"educ_level_m_2cat"] <- "univ_or_higher"
mid12$educ_level_m_3cat <- NULL
mid12$educ_level_m_2cat <- as.factor(mid12$educ_level_m_2cat)
load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/prenatal/Clinapsis/data/mid/16_Parto/Old/parto_v23_2024_06_26_AM.RData")
parto <- parto[, c("codigo_participante", "b_peso", "gestage_0y_c_weeks", "b_sexo")]
parto$b_sexo <- as.factor(parto$b_sexo)
df <- merge(mid12, parto, by = "codigo_participante", all.y = TRUE)

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/postnatal/18m/neuro/bayley/db/out/old/bayley_18m_AS_2024_05_03.RData")
db <- as.data.frame(db[, c("id_bisc")])
colnames(db) <- "id_bisc"
df_filtered <- merge(df, db, by.x = "codigo_participante", by.y = "id_bisc", all.y = TRUE)
df_filtered$group <- "bayley"
colnames(df_filtered) <- c("id_bisc","parity_m_2cat", "ethnicity_c_2cat", "educ_level_m_2cat",
                           "b_peso", "gestage_0y_c_weeks", "b_sexo", "group")

change <- which(startsWith(as.character(df_filtered$id_bisc), "10"))
df_filtered[change,"hosp"] <- "10"
change <- which(startsWith(as.character(df_filtered$id_bisc), "11"))
df_filtered[change,"hosp"] <- "11"
change <- which(startsWith(as.character(df_filtered$id_bisc), "12"))
df_filtered[change,"hosp"] <- "12"
df_filtered$hosp <- as.factor(df_filtered$hosp)
df_filtered$group <- as.factor(df_filtered$group)
df_filtered$id_bisc <- as.integer(df_filtered$id_bisc)

temp_env <- new.env()
load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/prenatal/32w/n_back/data/output/nback_32w_241202_v1_TG.RData", envir = temp_env)
assign("nback_32w", temp_env$df, envir = .GlobalEnv)
rm(temp_env)

library("readxl")
pma <- read_excel("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/prenatal/32w/pma_r/data/output/PMA_BiSC_32w_cleaned.xlsx")

pma <- pma[, c("id_mother", "pma_tcorrect_m_32w")]

db28m <- merge(db28m, pma, by.x = "id_bisc", by.y = "id_mother", all.x = TRUE)
fullsample <- merge(fullsample, pma, by.x = "id_bisc", by.y = "id_mother", all.x = TRUE)
df_filtered <-  merge(df_filtered, pma, by.x = "id_bisc", by.y = "id_mother", all.x = TRUE)

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/postnatal/breastfeeding_0y_to_18m/lactation_0to18m_PJ_20240126.RData")
lactation_0to18m <- lactation_0to18m[, c("id", "bf_c_18m")]

db28m <- merge(db28m, lactation_0to18m, by.x = "id_bisc", by.y = "id", all.x = TRUE)
fullsample <- merge(fullsample, lactation_0to18m, by.x = "id_bisc", by.y = "id", all.x = TRUE)
df_filtered <- merge(df_filtered, lactation_0to18m, by.x = "id_bisc", by.y = "id", all.x = TRUE)

fullsample <- subset(fullsample, select = -id_bisc)
df_filtered <- subset(df_filtered, select = -id_bisc)
fullsample <- rbind(fullsample, df_filtered)

rm(list = c("pma", "pma", "df", "db", "df_filtered", "mid12", "parto", "predbf6"))

library("readxl")
language <- read_excel("./db/20241011_ids_db28m.xlsx")
language <- language[, 1:2]
db28m <- merge(db28m, language, by.x = "id_bisc", by.y = "ids", all = TRUE)
db28m$idioma <- as.factor(db28m$idioma)


## 4) Descriptives:####################################################

library(xlsx)

db28m_cat <- subset(db28m, idioma == "cat" & !is.na(idioma)) # filter by idioma
db28m_esp <- subset(db28m, idioma == "esp" & !is.na(idioma))
db28m_full <- subset(db28m, !is.na(idioma))

calculate_summary <- function(x) {
  data.frame(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    na = sum(is.na(x))
  )
}

get_cont_stats <- function(data) { # function for continuous
  cont_cols <- sapply(data, is.numeric)
  res_list <- lapply(names(data)[cont_cols], function(col_name) {
    col_data <- data[[col_name]]
    stats <- calculate_summary(col_data)
    data.frame(variable = col_name, stats)
  })
  do.call(rbind, res_list)
}

get_factor_columns <- function(data) { #function for categorical
  factor_cols <- sapply(data, is.factor)
  colnames(data)[factor_cols]
}

get_cat_stats <- function(data) {
  cat_cols <- get_factor_columns(data)
  do.call(rbind, lapply(cat_cols, function(x) {
    res <- data.frame(table(data[, x], useNA = "always"))
    res$prop <- prop.table(table(data[, x], useNA = "always")) * 100
    res$variable <- x
    colnames(res) <- c("Category", "Freq", "%", "Var")
    res
  }))
}

res_cont_cat <- get_cont_stats(db28m_cat) #stats for each subset
res_cat_cat <- get_cat_stats(db28m_cat)

res_cont_esp <- get_cont_stats(db28m_esp)
res_cat_esp <- get_cat_stats(db28m_esp)

res_cont_full <- get_cont_stats(db28m_full)
res_cat_full <- get_cat_stats(db28m_full)

output_file <- paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_desc_db28m_by_idioma.xlsx") # save results in excel
write.xlsx(res_cont_cat, file = output_file, sheetName = "Continuous (Cat)", append = FALSE)
write.xlsx(res_cat_cat, file = output_file, sheetName = "Categorical (Cat)", append = TRUE)

write.xlsx(res_cont_esp, file = output_file, sheetName = "Continuous (Esp)", append = TRUE)
write.xlsx(res_cat_esp, file = output_file, sheetName = "Categorical (Esp)", append = TRUE)

write.xlsx(res_cont_full, file = output_file, sheetName = "Continuous (Full)", append = TRUE)
write.xlsx(res_cat_full, file = output_file, sheetName = "Categorical (Full)", append = TRUE)

rm(list = c("res_cont_cat", "res_cat_cat", "res_cont_esp", "res_cat_esp", "res_cont_full", "res_cat_full"))

# now for pvals baseline tab

library(tableone)
vars <- c("hosp", "parity_m_2cat", "ethnicity_c_2cat", "educ_level_m_2cat", "bf_c_18m",
          "b_peso", "gestage_0y_c_weeks", "b_sexo", "pma_tcorrect_m_32w", "age")
table1 <- CreateTableOne(vars = vars, strata = "group", data = fullsample, test = TRUE)
table1 <- as.data.frame(print(table1, smd = TRUE))
table1 <- cbind(col1 = rownames(table1), table1)
write.xlsx(table1, paste0("results/paper/tabs/", strftime(Sys.Date(),"%Y%m%d"), "_baseline_pvals_db28m_.xlsx"))

wilcox_test_result <- wilcox.test(gestage_0y_c_weeks ~ group, data = fullsample)
t_test_result <- t.test(b_peso ~ group, data = fullsample, var.equal = TRUE)
t_test_result_2 <- t.test(pma_tcorrect_m_32w ~ group, data = fullsample, var.equal = TRUE)

rm(list = c("fullsample", "table1", "change", "exclude_ids", "vars",
            "wilcox_test_result", "t_test_result", "t_test_result_2"))

# now for pvals baseline tab - IDIOMA

library(tableone)
library(xlsx)
vars <- c("hosp", "parity_m_2cat", "ethnicity_c_2cat", "educ_level_m_2cat", "bf_c_18m",
          "b_peso", "gestage_0y_c_weeks", "b_sexo", "pma_tcorrect_m_32w",
          "pd_igd", "pt_igd", "pd_motr", "pt_motr", "pd_cadapt", "pt_cadapt",
          "pd_socioem", "pt_socioem", "pd_cogn", "pt_cogn", "pd_com", "pt_com", "age")

table1 <- CreateTableOne(vars = vars, strata = "idioma", data = db28m, test = TRUE)
table1 <- as.data.frame(print(table1, smd = TRUE))

output_file <- paste0("results/paper/tabs/", strftime(Sys.Date(), "%Y%m%d"), "_baseline_pvals_db28m_by_idioma.xlsx")
write.xlsx(table1, file = output_file, sheetName = "Baseline_idioma", append = FALSE)

rm("table1")


## 5) Int. consistency / reliability (Cronbach's alpha):###############

library("readxl")
respmat28m <- read_excel("results/docs/28m_mat_resp_all_included_20240806.xlsx")
respmat28m$V194 <- NULL
respmat28m <- merge(respmat28m, language, by.x = 'dp3_28m$id_bisc', by.y = "ids", all.x = TRUE)
respmat28m$'dp3_28m$id_bisc' <- NULL
respmat28m_cat <- subset(respmat28m, idioma == "cat" & !is.na(idioma))
respmat28m_cat$'dp3_28m$id_bisc' <- NULL
respmat28m_esp <- subset(respmat28m, idioma == "esp" & !is.na(idioma))
respmat28m_esp$'dp3_28m$id_bisc' <- NULL

val_db_igd <- respmat28m[, 1:193]
val_db_motr <- respmat28m[, 1:40]
val_db_cadapt <- respmat28m[, 41:80]
val_db_socioem <- respmat28m[, 81:116]
val_db_cogn <- respmat28m[, 117:158]
val_db_com <- respmat28m[, 159:193]

val_db_igd_cat <- respmat28m_cat[, 1:193]
val_db_motr_cat <- respmat28m_cat[, 1:40]
val_db_cadapt_cat <- respmat28m_cat[, 41:80]
val_db_socioem_cat <- respmat28m_cat[, 81:116]
val_db_cogn_cat <- respmat28m_cat[, 117:158]
val_db_com_cat <- respmat28m_cat[, 159:193]

val_db_igd_esp <- respmat28m_esp[, 1:193]
val_db_motr_esp <- respmat28m_esp[, 1:40]
val_db_cadapt_esp <- respmat28m_esp[, 41:80]
val_db_socioem_esp <- respmat28m_esp[, 81:116]
val_db_cogn_esp <- respmat28m_esp[, 117:158]
val_db_com_esp <- respmat28m_esp[, 159:193]

library(psych)
library(foreign) 
library(lavaan)
library(semPlot)

# cronbach's alpha

val_db_igd_cronb <- psych::alpha(val_db_igd)
val_db_motr_cronb <- psych::alpha(val_db_motr)
val_db_cadapt_cronb <- psych::alpha(val_db_cadapt)
val_db_socioem_cronb <- psych::alpha(val_db_socioem)
val_db_cogn_cronb <- psych::alpha(val_db_cogn)
val_db_com_cronb <- psych::alpha(val_db_com)

val_db_igd_cronb_cat <- psych::alpha(val_db_igd_cat)
val_db_motr_cronb_cat <- psych::alpha(val_db_motr_cat)
val_db_cadapt_cronb_cat <- psych::alpha(val_db_cadapt_cat)
val_db_socioem_cronb_cat <- psych::alpha(val_db_socioem_cat)
val_db_cogn_cronb_cat <- psych::alpha(val_db_cogn_cat)
val_db_com_cronb_cat <- psych::alpha(val_db_com_cat)

val_db_igd_cronb_esp <- psych::alpha(val_db_igd_esp)
val_db_motr_cronb_esp <- psych::alpha(val_db_motr_esp)
val_db_cadapt_cronb_esp <- psych::alpha(val_db_cadapt_esp)
val_db_socioem_cronb_esp <- psych::alpha(val_db_socioem_esp)
val_db_cogn_cronb_esp <- psych::alpha(val_db_cogn_esp)
val_db_com_cronb_esp <- psych::alpha(val_db_com_esp)

cronbach_data <- data.frame(
  igd = val_db_igd_cronb$total[1],
  motr = val_db_motr_cronb$total[1],
  cadapt = val_db_cadapt_cronb$total[1],
  socioem = val_db_socioem_cronb$total[1],
  cogn = val_db_cogn_cronb$total[1],
  com = val_db_com_cronb$total[1]
  )
colnames(cronbach_data) <- c("igd", "motr", "cadapt", "socioem", "cogn", "com")

cronbach_data_cat <- data.frame(
  igd = val_db_igd_cronb_cat$total[1],
  motr = val_db_motr_cronb_cat$total[1],
  cadapt = val_db_cadapt_cronb_cat$total[1],
  socioem = val_db_socioem_cronb_cat$total[1],
  cogn = val_db_cogn_cronb_cat$total[1],
  com = val_db_com_cronb_cat$total[1]
)
colnames(cronbach_data_cat) <- c("igd", "motr", "cadapt", "socioem", "cogn", "com")

cronbach_data_esp <- data.frame(
  igd = val_db_igd_cronb_esp$total[1],
  motr = val_db_motr_cronb_esp$total[1],
  cadapt = val_db_cadapt_cronb_esp$total[1],
  socioem = val_db_socioem_cronb_esp$total[1],
  cogn = val_db_cogn_cronb_esp$total[1],
  com = val_db_com_cronb_esp$total[1]
)
colnames(cronbach_data_esp) <- c("igd", "motr", "cadapt", "socioem", "cogn", "com")

cronbach_data <- rbind(cronbach_data, cronbach_data_cat)
cronbach_data <- rbind(cronbach_data, cronbach_data_esp)
row.names(cronbach_data) <- c("full", "cat", "esp")

output_file <- paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_alphacronbach_db28m.xlsx")
write.xlsx(cronbach_data, file = output_file, sheetName = "Cronbach's Alpha", append = FALSE)

# guttmans lambda

library(mokken)

datasets <- list(val_db_igd, val_db_motr, val_db_cadapt, val_db_socioem, val_db_cogn, val_db_com)

lambda2_df <- data.frame(
  do.call(cbind, lapply(datasets, function(x) {
    check.reliability(x, MS = TRUE, alpha = TRUE, lambda.2 = TRUE, LCRC = FALSE, nclass = nclass.default)$lambda.2
  }))
)
colnames(lambda2_df) <- c("lambda2_igd", "lambda2_motr", "lambda2_cadapt", "lambda2_socioem", "lambda2_cogn", "lambda2_com")

write.csv(lambda2_df, paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_lambdaguttman_db28m.xlsx"), row.names = FALSE)


## 6) NAs per item:####################################################

respmat28m$zero_count <- rowSums(respmat28m == 0)

zero_per_column <- colSums(respmat28m == 0)
respmat28m <- rbind(respmat28m, zero_per_column)

write.xlsx(respmat28m, paste0("results/paper/tabs/", strftime(Sys.Date(),"%Y%m%d"), "_mat_resp_all_included_db28m_.xlsx"))

respmat28m$zero_count <- NULL
respmat28m$`dp3_28m$id_bisc` <- NULL
respmat28m <- respmat28m[1:440, ]

rm(list = c("val_db_cadapt", "val_db_cadapt_cronb", 
            "val_db_cogn", "val_db_cogn_cronb",
            "val_db_com", "val_db_com_cronb", 
            "val_db_igd", "val_db_igd_cronb", 
            "val_db_motr", "val_db_motr_cronb",
            "val_db_socioem", "val_db_socioem_cronb"))


## 7) CFA for dimensions:##############################################

cormatdata_pd <- db28m[ , c("pd_igd", "pd_motr", "pd_cadapt", "pd_socioem", "pd_cogn", "pd_com")]

formula  <- ' f  =~ pd_motr + pd_cadapt + pd_socioem + pd_cogn + pd_com'
cfa_1factor_5items_pd <- cfa(formula, data=cormatdata_pd) 
summary(cfa_1factor_5items_pd, fit.measures = TRUE, standardized = TRUE)

png(paste0("results/paper/figs/", strftime(Sys.Date(),"%Y%m%d"), "_cfa_dim_db28m.png"), width = 940, height = 670)  # You can adjust width and height as needed

semPaths(cfa_1factor_5items_pd, "par", 
         weighted = FALSE, 
         layout = "tree2", style = "ram", 
         sizeMan = 10, sizeLat = 10, shapeMan = "ellipse", 
         edge.color = "black",
         color = list(lat = "white", man = c("#FF7F50", "#FF7256", "#EE6A50", "#CD5B45", "#8B3E2F")),
         nodeLabels = c("   Motor   ", "Adapt. behav.", "Socioem.", "Cognitive", "  Comm.  ", "DP-3"),
         edge.label.cex = 1,
         node.label.cex = 1.5)

dev.off()

rm(list = c("cronbach_data", "cormatdata_pd"))

corplot <- as.data.frame(lavInspect(cfa_1factor_5items_pd, what = "sampstat"))
corplot$x <- row.names(corplot)
write.xlsx(corplot, paste0("results/paper/tabs/", strftime(Sys.Date(), "%Y%m%d"), "_corplot_dp3_db28m.xlsx"))

rm(list = c("cfa_1factor_5items_pd", "corplot"))


## 8) CFA for items:###################################################

old_colnames <- colnames(respmat28m)
new_colnames <- ifelse(grepl("^V[0-9]+$", old_colnames),
                       paste0("Digit", seq_along(old_colnames[grepl("^V[0-9]+$", old_colnames)])),
                       old_colnames)
colnames(respmat28m) <- new_colnames

items <- respmat28m[ , paste0("Digit", 1:193)] #nomes hi ha 193 items
items[items == 0] <- NA #els que tinguin 0 en realitat son NA
items_cc <- items[complete.cases(items), ] #esborrem si tenen cap NA
items_cc[apply(items_cc, 2, function(x) all(x == 2))] <- NULL #elimina les columnes que tenen tot "no", es a dir, les que tot 2
items_cc[apply(items_cc, 2, function(x) all(x == 1))] <- NULL #elimina les columnes que tenen tot "si", es a dir, les que tot 1

formula  <- ' f  =~ Digit5 + Digit6 + Digit8 + Digit9 + Digit10 + Digit11 + Digit12 + Digit13 + Digit14 + Digit15 + Digit16 + Digit17 + Digit18 + Digit19 + Digit20 + Digit21 + Digit22 + Digit23 + Digit24 + Digit25 + Digit26 + Digit27 + Digit28 + Digit29 + Digit30 + Digit31 + Digit32 + Digit33 + Digit34 + Digit35 + Digit36 + Digit37 + Digit38 + Digit39 + Digit40 + Digit41 + Digit42 + Digit50 + Digit51 + Digit52 + Digit53 + Digit54 + Digit55 + Digit56 + Digit57 + Digit58 + Digit59 + Digit60 + Digit61 + Digit62 + Digit63 + Digit64 + Digit65 + Digit66 + Digit67 + Digit68 + Digit69 + Digit70 + Digit71 + Digit72 + Digit73 + Digit74 + Digit75 + Digit76 + Digit79 + Digit82 + Digit83 + Digit84 + Digit85 + Digit86 + Digit87 + Digit88 + Digit89 + Digit90 + Digit91 + Digit92 + Digit93 + Digit94 + Digit95 + Digit96 + Digit97 + Digit98 + Digit99 + Digit100 + Digit101 + Digit102 + Digit103 + Digit104 + Digit105 + Digit106 + Digit107 + Digit108 + Digit109 + Digit110 + Digit111 + Digit112 + Digit113 + Digit114 + Digit118 + Digit120 + Digit121 + Digit123 + Digit124 + Digit125 + Digit126 + Digit127 + Digit128 + Digit129 + Digit130 + Digit131 + Digit132 + Digit133 + Digit134 + Digit135 + Digit136 + Digit137 + Digit138 + Digit139 + Digit140 + Digit141 + Digit142 + Digit143 + Digit144 + Digit145 + Digit146 + Digit147 + Digit148 + Digit149 + Digit150 + Digit160 + Digit161 + Digit162 + Digit163 + Digit165 + Digit166 + Digit167 + Digit168 + Digit169 + Digit170 + Digit171 + Digit172 + Digit173 + Digit174 + Digit175 + Digit176 + Digit177 + Digit178 + Digit179 + Digit180 + Digit181 + Digit182 + Digit183 + Digit184 + Digit185 + Digit186 + Digit187 + Digit191 '
# ERROR, CHECK MULTICOLLINEARITY

# to check multicollinearity
cor_matrix <- cor(items_cc, use = "complete.obs")
high_cor <- which(abs(cor_matrix) > 0.95, arr.ind = TRUE)
if(length(high_cor) > 0) {
  print(high_cor)
} else {
  print("No highly correlated variables")
}

#remove highly correlated items
items_cc <- items_cc[, !colnames(items_cc) %in% c("Digit6", "Digit87", "Digit123", "Digit163", "Digit167")]
formula  <- ' f  =~ Digit5 + Digit8 + Digit9 + Digit10 + Digit11 + Digit12 + Digit13 + Digit14 + Digit15 + Digit16 + Digit17 + Digit18 + Digit19 + Digit20 + Digit21 + Digit22 + Digit23 + Digit24 + Digit25 + Digit26 + Digit27 + Digit28 + Digit29 + Digit30 + Digit31 + Digit32 + Digit33 + Digit34 + Digit35 + Digit36 + Digit37 + Digit38 + Digit39 + Digit40 + Digit41 + Digit42 + Digit50 + Digit51 + Digit52 + Digit53 + Digit54 + Digit55 + Digit56 + Digit57 + Digit58 + Digit59 + Digit60 + Digit61 + Digit62 + Digit63 + Digit64 + Digit65 + Digit66 + Digit67 + Digit68 + Digit69 + Digit70 + Digit71 + Digit72 + Digit73 + Digit74 + Digit75 + Digit76 + Digit79 + Digit82 + Digit83 + Digit84 + Digit85 + Digit86 + Digit88 + Digit89 + Digit90 + Digit91 + Digit92 + Digit93 + Digit94 + Digit95 + Digit96 + Digit97 + Digit98 + Digit99 + Digit100 + Digit101 + Digit102 + Digit103 + Digit104 + Digit105 + Digit106 + Digit107 + Digit108 + Digit109 + Digit110 + Digit111 + Digit112 + Digit113 + Digit114 + Digit118 + Digit120 + Digit121 + Digit124 + Digit125 + Digit126 + Digit127 + Digit128 + Digit129 + Digit130 + Digit131 + Digit132 + Digit133 + Digit134 + Digit135 + Digit136 + Digit137 + Digit138 + Digit139 + Digit140 + Digit141 + Digit142 + Digit143 + Digit144 + Digit145 + Digit146 + Digit147 + Digit148 + Digit149 + Digit150 + Digit160 + Digit161 + Digit162 + Digit165 + Digit166 + Digit168 + Digit169 + Digit170 + Digit171 + Digit172 + Digit173 + Digit174 + Digit175 + Digit176 + Digit177 + Digit178 + Digit179 + Digit180 + Digit181 + Digit182 + Digit183 + Digit184 + Digit185 + Digit186 + Digit187 + Digit191 '
# ERROR AGAIN

# not enough, we lower the threshold
cor_matrix <- cor(items_cc, use = "complete.obs")
high_cor <- which(abs(cor_matrix) > 0.90, arr.ind = TRUE)
# Remove one item from each pair
to_remove <- unique(rownames(high_cor)[high_cor[, 1] != high_cor[, 2]])
items_cc <- items_cc[, !colnames(items_cc) %in% to_remove]
formula  <- ' f  =~ Digit5 + Digit12 + Digit13 + Digit14 + Digit15 + Digit16 + Digit17 + Digit18 + Digit19 + Digit20 + Digit21 + Digit22 + Digit23 + Digit24 + Digit25 + Digit26 + Digit27 + Digit28 + Digit29 + Digit30 + Digit31 + Digit32 + Digit33 + Digit34 + Digit35 + Digit36 + Digit37 + Digit38 + Digit39 + Digit40 + Digit41 + Digit42 + Digit50 + Digit51 + Digit52 + Digit53 + Digit54 + Digit55 + Digit56 + Digit57 + Digit58 + Digit59 + Digit60 + Digit61 + Digit62 + Digit63 + Digit64 + Digit65 + Digit66 + Digit67 + Digit68 + Digit69 + Digit70 + Digit71 + Digit72 + Digit73 + Digit74 + Digit75 + Digit76 + Digit79 + Digit82 + Digit83 + Digit84 + Digit85 + Digit86 + Digit88 + Digit89 + Digit90 + Digit91 + Digit92 + Digit93 + Digit94 + Digit95 + Digit96 + Digit97 + Digit98 + Digit99 + Digit100 + Digit101 + Digit102 + Digit103 + Digit104 + Digit105 + Digit106 + Digit107 + Digit108 + Digit109 + Digit110 + Digit111 + Digit112 + Digit113 + Digit114 + Digit118 + Digit120 + Digit121 + Digit124 + Digit125 + Digit126 + Digit127 + Digit128 + Digit129 + Digit130 + Digit131 + Digit132 + Digit133 + Digit134 + Digit135 + Digit136 + Digit137 + Digit138 + Digit139 + Digit140 + Digit141 + Digit142 + Digit143 + Digit144 + Digit145 + Digit146 + Digit147 + Digit148 + Digit149 + Digit150 + Digit160 + Digit161 + Digit162 + Digit165 + Digit166 + Digit168 + Digit169 + Digit170 + Digit171 + Digit172 + Digit173 + Digit174 + Digit175 + Digit176 + Digit177 + Digit178 + Digit179 + Digit180 + Digit181 + Digit182 + Digit183 + Digit184 + Digit185 + Digit186 + Digit187 + Digit191 '
cfa_1factor_5items_it <- cfa(formula, data=items_cc)
summary(cfa_1factor_5items_it, fit.measures = TRUE, standardized = TRUE) # ERROR AGAIN

#semPaths(cfa_1factor_5items_it, "par", weighted = FALSE, nCharNodes = 8, shapeMan = "rectangle",
#         sizeMan = 10, sizeMan2 = 8)

#lavInspect(cfa_1factor_5items_it, what = "sampstat")

rm(list = c("items", "items_cc", "respmat28m"))


## 9) Assess normality:################################################

variables <- c("pd_igd", "pd_motr", "pd_cadapt", "pd_socioem", "pd_cogn", "pd_com")

png(paste0("results/paper/figs/", strftime(Sys.Date(),"%Y%m%d"), "_normality_qq_db28m.png"), width = 1100, height = 700)  # adjust width / height as needed
par(mfrow = c(2, 3))
for (var in variables) {
  qqnorm(db28m[[var]], main = var, pch = 1, frame = FALSE)
  qqline(db28m[[var]], col = "steelblue", lwd = 2)
}
dev.off()


## 10) Breastfeeding reference category:###############################

summary(db28m$bf_c_18m)
db28m$bf_c_18m <- relevel(db28m$bf_c_18m, ref = "1")


## 11) Bivariate analyses:#############################################

db28m$parity_m_2cat <- factor(db28m$parity_m_2cat, levels = c("nulliparous", "multiparous"))

exposures <- c("age", "b_sexo", "parity_m_2cat", "ethnicity_c_2cat", 
               "educ_level_m_2cat", "b_peso", "gestage_0y_c_weeks", 
               "pma_tcorrect_m_32w", "bf_c_18m")

# function for lm unadjusted
fit_lm_model <- function(data, outcome, exposure) {
  formula <- as.formula(paste(outcome, "~", exposure))
  model <- lm(formula, data = data)
  result <- tidy(model, conf.int = TRUE)
  result$outcome <- outcome
  result$exposure <- exposure
  result$model_type <- "lm"
  return(list(result = result, model = model))
}

library(broom)
library(ggplot2)
library(openxlsx)

# initialize lm_results_list
lm_results_list <- list()
lm_models <- list()

# run lm and save qqplots
for (outcome in variables) {
  for (exposure in exposures) {
    model_result <- tryCatch({
      fit_lm_model(db28m, outcome, exposure)
    }, error = function(e) {
      message(paste("Error in lm model with outcome:", outcome, "and exposure:", exposure, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(model_result)) {
      lm_results_list <- append(lm_results_list, list(model_result$result)) #save results and model
      lm_models <- append(lm_models, list(model_result$model))
      residuals <- resid(model_result$model) #generate qqplot
      qq_plot <- ggplot(data = data.frame(sample = residuals), aes(sample = sample)) +
        stat_qq() +
        stat_qq_line() +
        ggtitle(paste("QQ Plot for", outcome, "vs", exposure))
      ggsave(filename = paste0("results/paper/figs/qqplots_models/bivar/", format(Sys.Date(), "%Y%m%d"), "_qqplots_", outcome, "_vs_", exposure, "_db28m.png"), plot = qq_plot)
    }
  }
}

lm_results <- do.call(rbind, lm_results_list) #combine results into dfs
colnames(lm_results) <- c("term", "estimate", "std_error", "statistic", "p_value", "conf_low", "conf_high", "outcome", "exposure", "model_type")
write.xlsx(lm_results, file = paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_bivar_db28m.xlsx"), sheetName = "Sheet1")


## 12) Multivariate analyses:##########################################

exposures <- c("parity_m_2cat", "ethnicity_c_2cat", 
               "educ_level_m_2cat", "b_peso", "gestage_0y_c_weeks", 
               "pma_tcorrect_m_32w", "bf_c_18m")

# function for lm adjusted
fit_lm_model <- function(data, outcome, exposure) {
  formula <- as.formula(paste(outcome, "~", exposure, "+ age + b_sexo"))
  model <- lm(formula, data = data)
  result <- tidy(model, conf.int = TRUE)
  result$outcome <- outcome
  result$exposure <- exposure
  result$model_type <- "lm"
  return(list(result = result, model = model))
}

library(broom)
library(ggplot2)
library(openxlsx)

# initialize lm_results_list
lm_results_list <- list()
lm_models <- list()

# run lm and save qqplots
for (outcome in variables) {
  for (exposure in exposures) {
    model_result <- tryCatch({
      fit_lm_model(db28m, outcome, exposure)
    }, error = function(e) {
      message(paste("Error in lm model with outcome:", outcome, "and exposure:", exposure, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(model_result)) {
      lm_results_list <- append(lm_results_list, list(model_result$result)) #save results and model
      lm_models <- append(lm_models, list(model_result$model))
      residuals <- resid(model_result$model) #generate qqplot
      qq_plot <- ggplot(data = data.frame(sample = residuals), aes(sample = sample)) +
        stat_qq() +
        stat_qq_line() +
        ggtitle(paste("QQ Plot for", outcome, "vs", exposure))
      ggsave(filename = paste0("results/paper/figs/qqplots_models/multivar/", format(Sys.Date(), "%Y%m%d"), "_qqplots_", outcome, "_vs_", exposure, "_db28m.png"), plot = qq_plot)
    }
  }
}

lm_results <- do.call(rbind, lm_results_list) #combine results into dfs
colnames(lm_results) <- c("term", "estimate", "std_error", "statistic", "p_value", "conf_low", "conf_high", "outcome", "exposure", "model_type")
write.xlsx(lm_results, file = paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_multivar_db28m.xlsx"), sheetName = "Sheet1")

rm(list = c("lm_models", "lm_results", "lm_results_list", "model_result", "qq_plot"))


## 13) To get the eta squared:#########################################

exposures <- c("b_sexo", "age", "parity_m_2cat", "ethnicity_c_2cat", 
               "educ_level_m_2cat", "b_peso", "gestage_0y_c_weeks", 
               "pma_tcorrect_m_32w", "bf_c_18m")

library(tibble)

lm_results_list <- list()

calculate_eta_squared <- function(model) { # function for eta squared calculation
  anova_result <- anova(model)
  eta_squared <- anova_result[["Sum Sq"]][1] / sum(anova_result[["Sum Sq"]])
  return(eta_squared)
}

for (outcome in variables) { #loop outc-exp
  for (exposure in exposures) {
    model <- tryCatch({ # fit lm
      lm(as.formula(paste(outcome, "~", exposure)), data = db28m)
    }, error = function(e) {
      message(paste("Error in lm model with outcome:", outcome, "and exposure:", exposure, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(model)) {
      model_summary <- summary(model) # to get betas
      betas <- coef(model_summary)
      
      results_df <- as_tibble(betas, rownames = "term") # put betas in a tab
      results_df$outcome <- outcome
      results_df$exposure <- exposure
      results_df$model_type <- "lm"
      
      eta_sq <- calculate_eta_squared(model) # add etas in the tab
      results_df$eta_squared <- eta_sq
      
      lm_results_list <- append(lm_results_list, list(results_df))
      message(paste("Model:", outcome, "~", exposure, "Eta Squared:", eta_sq))
    }
  }
}

lm_results_df <- do.call(rbind, lm_results_list) #combine into df
write.xlsx(lm_results_df, file = paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_bivar_db28m_ETA.xlsx"), sheetName = "Sheet1")

### MULTIVAR

exposures <- c("parity_m_2cat", "ethnicity_c_2cat", 
               "educ_level_m_2cat", "b_peso", "gestage_0y_c_weeks", 
               "pma_tcorrect_m_32w", "bf_c_18m")

library(tibble)

lm_results_list <- list()

calculate_eta_squared <- function(model) { # function for eta squared calculation
  anova_result <- anova(model)
  eta_squared <- anova_result[["Sum Sq"]][1] / sum(anova_result[["Sum Sq"]])
  return(eta_squared)
}

for (outcome in variables) { #loop outc-exp
  for (exposure in exposures) {
    model <- tryCatch({ # fit lm
      lm(as.formula(paste(outcome, "~", exposure, "+ age + b_sexo")), data = db28m)
    }, error = function(e) {
      message(paste("Error in lm model with outcome:", outcome, "and exposure:", exposure, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(model)) {
      model_summary <- summary(model) # to get betas
      betas <- coef(model_summary)
      
      results_df <- as_tibble(betas, rownames = "term") # put betas in a tab
      results_df$outcome <- outcome
      results_df$exposure <- exposure
      results_df$model_type <- "lm"
      
      eta_sq <- calculate_eta_squared(model) # add etas in the tab
      results_df$eta_squared <- eta_sq
      
      lm_results_list <- append(lm_results_list, list(results_df))
      message(paste("Model:", outcome, "~", exposure, "Eta Squared:", eta_sq))
    }
  }
}

lm_results_df <- do.call(rbind, lm_results_list) #combine into df
write.xlsx(lm_results_df, file = paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_multivar_db28m_ETA.xlsx"), sheetName = "Sheet1")


## 14) ICC with Bayley 18m:############################################

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/postnatal/18m/neuro/bayley/db/out/old/bayley_18m_AS_2024_05_03.RData")
bayley18 <- db[, c("id_bisc", "date_bayley_c_18m", "pdg_bayley_c_18m", 
                    "pdcr_bayley_c_18m", "pdce_bayley_c_18m", "pdmf_bayley_c_18m",
                    "pdmg_bayley_c_18m", "pdg_prem_bayley_c_18m", 
                    "pdcr_prem_bayley_c_18m", "pdce_prem_bayley_c_18m", 
                    "pdmf_prem_bayley_c_18m", "pdmg_prem_bayley_c_18m")]
rm(db)

bayley18$pdg_prem_bayley_c_18m[is.na(bayley18$pdg_prem_bayley_c_18m)] <- 
  bayley18$pdg_bayley_c_18m[is.na(bayley18$pdg_prem_bayley_c_18m)]
bayley18$pdcr_prem_bayley_c_18m[is.na(bayley18$pdcr_prem_bayley_c_18m)] <- 
  bayley18$pdcr_bayley_c_18m[is.na(bayley18$pdcr_prem_bayley_c_18m)]
bayley18$pdce_prem_bayley_c_18m[is.na(bayley18$pdce_prem_bayley_c_18m)] <- 
  bayley18$pdce_bayley_c_18m[is.na(bayley18$pdce_prem_bayley_c_18m)]
bayley18$pdmf_prem_bayley_c_18m[is.na(bayley18$pdmf_prem_bayley_c_18m)] <- 
  bayley18$pdmf_bayley_c_18m[is.na(bayley18$pdmf_prem_bayley_c_18m)]
bayley18$pdmg_prem_bayley_c_18m[is.na(bayley18$pdmg_prem_bayley_c_18m)] <- 
  bayley18$pdmg_bayley_c_18m[is.na(bayley18$pdmg_prem_bayley_c_18m)]

db28m_bayley18 <- merge(db28m, bayley18, by = "id_bisc")
bay_dp3_cogn <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_cogn", "pdg_bayley_c_18m")]), c("pd_cogn", "pdg_bayley_c_18m")]
bay_dp3_mf <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_motr", "pdmf_bayley_c_18m")]), c("pd_motr", "pdmf_bayley_c_18m")]
bay_dp3_mg <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_motr", "pdmg_bayley_c_18m")]), c("pd_motr", "pdmg_bayley_c_18m")]
bay_dp3_cr <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_com", "pdcr_bayley_c_18m")]), c("pd_com", "pdcr_bayley_c_18m")]
bay_dp3_ce <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_com", "pdce_bayley_c_18m")]), c("pd_com", "pdce_bayley_c_18m")]

library(irr)
library(openxlsx)

icc_list <- list(
  bay_dp3_cogn = icc(bay_dp3_cogn, model = "twoway", type = "agreement", unit = "average"), # run icc
  bay_dp3_mf = icc(bay_dp3_mf, model = "twoway", type = "agreement", unit = "average"),
  bay_dp3_mg = icc(bay_dp3_mg, model = "twoway", type = "agreement", unit = "average"),
  bay_dp3_cr = icc(bay_dp3_cr, model = "twoway", type = "agreement", unit = "average"),
  bay_dp3_ce = icc(bay_dp3_ce, model = "twoway", type = "agreement", unit = "average")
)

icc_results <- data.frame( # extract relevant results into a df
  Dataset = names(icc_list),
  Subjects = sapply(icc_list, function(x) x$subjects),
  Raters = sapply(icc_list, function(x) x$raters),
  ICC = sapply(icc_list, function(x) x$value),
  F_Value = sapply(icc_list, function(x) x$Fvalue),
  p_Value = sapply(icc_list, function(x) x$p.value),
  CI_Lower = sapply(icc_list, function(x) x$lbound),
  CI_Upper = sapply(icc_list, function(x) x$ubound)
)

write.xlsx(icc_results, file = paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_icc_bayley18_dp3_db28m.xlsx"), sheetName = "Sheet1")

# same but for corrected for prematurity
bay_dp3_cogn <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_cogn", "pdg_prem_bayley_c_18m")]), c("pd_cogn", "pdg_prem_bayley_c_18m")]
bay_dp3_mf <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_motr", "pdmf_prem_bayley_c_18m")]), c("pd_motr", "pdmf_prem_bayley_c_18m")]
bay_dp3_mg <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_motr", "pdmg_prem_bayley_c_18m")]), c("pd_motr", "pdmg_prem_bayley_c_18m")]
bay_dp3_cr <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_com", "pdcr_prem_bayley_c_18m")]), c("pd_com", "pdcr_prem_bayley_c_18m")]
bay_dp3_ce <- db28m_bayley18[complete.cases(db28m_bayley18[, c("pd_com", "pdce_prem_bayley_c_18m")]), c("pd_com", "pdce_prem_bayley_c_18m")]

library(irr)
library(openxlsx)

icc_list <- list(
  bay_dp3_cogn = icc(bay_dp3_cogn, model = "twoway", type = "agreement", unit = "average"), # run icc
  bay_dp3_mf = icc(bay_dp3_mf, model = "twoway", type = "agreement", unit = "average"),
  bay_dp3_mg = icc(bay_dp3_mg, model = "twoway", type = "agreement", unit = "average"),
  bay_dp3_cr = icc(bay_dp3_cr, model = "twoway", type = "agreement", unit = "average"),
  bay_dp3_ce = icc(bay_dp3_ce, model = "twoway", type = "agreement", unit = "average")
)

icc_results <- data.frame( # extract relevant results into a df
  Dataset = names(icc_list),
  Subjects = sapply(icc_list, function(x) x$subjects),
  Raters = sapply(icc_list, function(x) x$raters),
  ICC = sapply(icc_list, function(x) x$value),
  F_Value = sapply(icc_list, function(x) x$Fvalue),
  p_Value = sapply(icc_list, function(x) x$p.value),
  CI_Lower = sapply(icc_list, function(x) x$lbound),
  CI_Upper = sapply(icc_list, function(x) x$ubound)
)

write.xlsx(icc_results, file = paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_icc_bayley18_prem_dp3_db28m.xlsx"), sheetName = "Sheet1")

rm(list = c("bay_dp3_ce", "bay_dp3_cogn", "bay_dp3_cr", "bay_dp3_mf", "bay_dp3_mg",
            "icc_list", "icc_results"))


## 15) Correlation plot DP-3_28m:######################################

library(PerformanceAnalytics)
library(ggplot2)
library(GGally)
library(psych)

cormatdata_pd <- db28m_bayley18[ , c("pd_motr", "pd_cadapt", "pd_socioem", "pd_cogn", "pd_com", 
                                     "pdg_bayley_c_18m", "pdcr_bayley_c_18m",
                                     "pdce_bayley_c_18m", "pdmf_bayley_c_18m", "pdmg_bayley_c_18m")]

library(devEMF)
emf(file = paste0("results/paper/figs/", format(Sys.Date(), "%Y%m%d"), "_corrplot_dim_dp3_db28m.emf"), 
    width = 10, height = 10)

pairs.panels(cormatdata_pd,
             smooth = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = TRUE,
             method = "spearman",
             pch = 21,
             lm = FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 2,
             stars = FALSE,
             ci = TRUE)

dev.off()

rm(list = c("cormatdata_pd", "db28m_bayley18", "respmat28m"))


## 16) Association Bayley_18m - DP-3_28m:##############################

load("//FS05.isglobal.lan/HPC_BISC_DATA/data_management/data_general/prenatal/Clinapsis/data/mid/16_Parto/Old/parto_v23_2024_06_26_AM.RData")
parto <- parto[, c("codigo_participante", "f_parto")]
bayley18 <- merge(bayley18, parto, by.x = "id_bisc", by.y = "codigo_participante", all.x = TRUE)
bayley18$date_bayley_c_18m <- as.Date(bayley18$date_bayley_c_18m)
bayley18$f_parto <- as.Date(bayley18$f_parto)
bayley18$age18 <- as.numeric(bayley18$date_bayley_c_18m - bayley18$f_parto)
merged <- merge(db28m, bayley18, by = "id_bisc", all.x = TRUE)

library(broom)
library(openxlsx)

# Define the models
models_list <- list(
  pd_motr1 = "pd_motr ~ pdmf_bayley_c_18m",
  pd_motr2 = "pd_motr ~ pdmf_bayley_c_18m + age + age18 + b_sexo",
  pd_motr3 = "pd_motr ~ pdmg_bayley_c_18m",
  pd_motr4 = "pd_motr ~ pdmg_bayley_c_18m + age + age18 + b_sexo",
  pd_com1 = "pd_com ~ pdcr_bayley_c_18m",
  pd_com2 = "pd_com ~ pdcr_bayley_c_18m + age + age18 + b_sexo",
  pd_com3 = "pd_com ~ pdce_bayley_c_18m",
  pd_com4 = "pd_com ~ pdce_bayley_c_18m + age + age18 + b_sexo",
  pd_cogn1 = "pd_cogn ~ pdg_bayley_c_18m",
  pd_cogn2 = "pd_cogn ~ pdg_bayley_c_18m + age + age18 + b_sexo",
  pd_igd1 = "pd_igd ~ pdmf_bayley_c_18m",
  pd_igd2 = "pd_igd ~ pdmf_bayley_c_18m + age + age18 + b_sexo",
  pd_igd3 = "pd_igd ~ pdmg_bayley_c_18m",
  pd_igd4 = "pd_igd ~ pdmg_bayley_c_18m + age + age18 + b_sexo",
  pd_igd5 = "pd_igd ~ pdcr_bayley_c_18m",
  pd_igd6 = "pd_igd ~ pdcr_bayley_c_18m + age + age18 + b_sexo",
  pd_igd7 = "pd_igd ~ pdce_bayley_c_18m",
  pd_igd8 = "pd_igd ~ pdce_bayley_c_18m + age + age18 + b_sexo",
  pd_igd9 = "pd_igd ~ pdg_bayley_c_18m",
  pd_igd10 = "pd_igd ~ pdg_bayley_c_18m + age + age18 + b_sexo"
)

results_list <- list()
eta_squared_list <- list()

for (model_name in names(models_list)) {
  formula <- as.formula(models_list[[model_name]])
  model <- lm(formula, data = merged)
  
  # Get regression coefficients
  model_result <- tidy(model, conf.int = TRUE)
  model_result$model_name <- model_name
  results_list[[model_name]] <- model_result
  
  # Compute eta squared manually
  anova_result <- anova(model)
  ss_total <- sum(anova_result[["Sum Sq"]])
  eta_sq <- data.frame(
    model_name = model_name,
    term = rownames(anova_result),
    eta_squared = anova_result[["Sum Sq"]] / ss_total
  )
  
  eta_squared_list[[model_name]] <- eta_sq
}

# Combine results
all_results <- do.call(rbind, results_list)
colnames(all_results) <- c("term", "estimate", "std_error", "statistic", "p_value", "conf_low", "conf_high", "model_name")
eta_results <- do.call(rbind, eta_squared_list)

# Merge coefficients and eta squared results
final_results <- merge(all_results, eta_results, by = c("model_name", "term"), all.x = TRUE)

# Save to Excel
wb <- createWorkbook()
addWorksheet(wb, "Regression Results")
writeData(wb, "Regression Results", final_results)
saveWorkbook(wb, paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_models_bayley18_dp3_db28m.xlsx"), overwrite = TRUE)

# Clean up
rm(list = c("models_list", "results_list", "model_name", "formula", "model", "model_result", "all_results", "wb"))


## 17) Sensitivity to change:##########################################

db8m <- db8m[-which(db8m$id_bisc == "11002211"), c("id_bisc", "pd_igd", "pd_motr", 
                                                   "pd_cadapt", "pd_socioem", "pd_cogn", "pd_com",
                                                   "age_dp3")] # 11002211 exluded for data quality reasons

sens_change <- merge(db8m, db28m, by = "id_bisc")
colnames(sens_change) <- c("id_bisc", "pd_igd_8m", "pd_motr_8m", "pd_cadapt_8m", 
                           "pd_socioem_8m", "pd_cogn_8m", "pd_com_8m", "age_dp3",
                           "Aplicador", "pd_inc", "pt_inc", "pd_igd_28m", "pt_igd",
                           "pd_motr_28m", "pt_motr", "pd_cadapt_28m", "pt_cadapt",
                           "pd_socioem_28m", "pt_socioem", "pd_cogn_28m", "pt_cogn",
                           "pd_com_28m", "pt_com", "age", "hosp", "parity_m_2cat",
                           "ethnicity_c_2cat", "educ_level_m_2cat", "b_peso",
                           "gestage_0y_c_weeks", "b_sexo", "dnumeros3_m_32w",
                           "dnumeros4_m_32w", "pma_tcorrect_m_32w", "bf_c_18m",
                           "idioma")
sens_change <- sens_change[, c("id_bisc", "pd_igd_8m", "pd_motr_8m", "pd_cadapt_8m", 
                               "pd_socioem_8m", "pd_cogn_8m", "pd_com_8m", "age_dp3",
                               "pd_igd_28m", "pd_motr_28m", "pd_cadapt_28m",
                               "pd_socioem_28m", "pd_cogn_28m", "pd_com_28m", 
                               "age", "hosp", "parity_m_2cat",
                               "ethnicity_c_2cat", "educ_level_m_2cat", "b_peso",
                               "gestage_0y_c_weeks", "b_sexo", "dnumeros3_m_32w",
                               "dnumeros4_m_32w", "pma_tcorrect_m_32w", "bf_c_18m",
                               "idioma")]

sens_change$age_dp3 <- sens_change$age_dp3*0.0329

# 8m descriptives


sens_change$bf_c_18m <- factor(sens_change$bf_c_18m, levels = c("1", "0", "never_bf"))
sens_change$bf_c_18m <- addNA(sens_change$bf_c_18m)

calculate_summary <- function(x) {
  data.frame(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    na = sum(is.na(x))
  )
}

get_cont_stats <- function(data) { # function for continuous
  cont_cols <- sapply(data, is.numeric)
  res_list <- lapply(names(data)[cont_cols], function(col_name) {
    col_data <- data[[col_name]]
    stats <- calculate_summary(col_data)
    data.frame(variable = col_name, stats)
  })
  do.call(rbind, res_list)
}

get_factor_columns <- function(data) { #function for categorical
  factor_cols <- sapply(data, is.factor)
  colnames(data)[factor_cols]
}

get_cat_stats <- function(data) {
  cat_cols <- get_factor_columns(data)
  do.call(rbind, lapply(cat_cols, function(x) {
    res <- data.frame(table(data[, x], useNA = "always"))
    res$prop <- prop.table(table(data[, x], useNA = "always")) * 100
    res$variable <- x
    colnames(res) <- c("Category", "Freq", "%", "Var")
    res
  }))
}

res_cont_full <- get_cont_stats(sens_change)
res_cat_full <- get_cat_stats(sens_change)

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Continuous (Cat)")
addWorksheet(wb, "Categorical (Cat)")
writeData(wb, sheet = "Continuous (Cat)", x = res_cont_full)
writeData(wb, sheet = "Categorical (Cat)", x = res_cat_full)
output_file <- paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_desc_db8m_senschange.xlsx")
saveWorkbook(wb, file = output_file, overwrite = TRUE)

library(ggplot2)
library(cowplot)

variable_pairs <- list( # define vars
  motr = c("pd_motr_28m", "pd_motr_8m"),
  cadapt = c("pd_cadapt_28m", "pd_cadapt_8m"),
  socioem = c("pd_socioem_28m", "pd_socioem_8m"),
  cogn = c("pd_cogn_28m", "pd_cogn_8m"),
  com = c("pd_com_28m", "pd_com_8m")
)

custom_colors <- c("1" = "#843B85", "0" = "#E366E6", "never_bf" = "#C8BFE7")

bf_plots <- list()

all_changes <- unlist(lapply(names(variable_pairs), function(var) {
  sens_change[[variable_pairs[[var]][1]]] - sens_change[[variable_pairs[[var]][2]]]
}))
y_range <- range(all_changes, na.rm = TRUE)

for (var in names(variable_pairs)) { # change variable
  sens_change[[paste0(var, "_change")]] <- sens_change[[variable_pairs[[var]][1]]] - 
    sens_change[[variable_pairs[[var]][2]]]
  
  plot_data <- sens_change # separate variable for plotting to avoid overwritting
  plot_data$change_var <- plot_data[[paste0(var, "_change")]]
  
  bf_plots[[var]] <- ggplot(plot_data, aes(x = bf_c_18m, y = change_var, fill = bf_c_18m)) +
    geom_boxplot(outlier.shape = NA) + # Boxplot without default outliers
    geom_jitter(aes(color = bf_c_18m), width = 0.2, size = 1.5, alpha = 0.6) + # Add jittered points
    scale_fill_manual(values = custom_colors) +
    scale_color_manual(values = custom_colors) + # Ensure consistency in fill and point colors
    scale_y_continuous(limits = y_range) + # Set the same y-axis range for all plots
    labs(
      x = "BF_C_18M", 
      y = paste("Change in", var, "Score"), 
      title = paste("Change in", toupper(var), "by BF_C_18M")
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# plots into a single panel
bf_panel <- plot_grid(plotlist = bf_plots, ncol = 3)

ggsave(paste0("results/paper/figs/", format(Sys.Date(), "%Y%m%d"), "_senschange_bf_dots.png"), bf_panel, width = 18, height = 8)

# to get the pvals
library(writexl)
outcomes <- c("motr_change", "cadapt_change", "socioem_change", "cogn_change", "com_change")
predictors <- c("pd_motr_8m", "pd_cadapt_8m", "pd_socioem_8m", "pd_cogn_8m", "pd_com_8m")

lm_results_list <- list()

for (i in 1:length(outcomes)) {
  formula <- as.formula(paste(outcomes[i], "~ bf_c_18m +", predictors[i], "+ age + age_dp3 + b_sexo"))
  lm_model <- lm(formula, data = sens_change)
  lm_summary <- summary(lm_model)
  lm_summary_df <- data.frame(
    Term = rownames(lm_summary$coefficients),
    Estimate = lm_summary$coefficients[, 1],
    Std_Error = lm_summary$coefficients[, 2],
    t_Value = lm_summary$coefficients[, 3],
    Pr_Value = lm_summary$coefficients[, 4]
  )
  lm_results_list[[outcomes[i]]] <- lm_summary_df
}

write_xlsx(lm_results_list, paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_senschange_bf.xlsx"))

# cohens effect

levels(sens_change$bf_c_18m) <- c(levels(sens_change$bf_c_18m), "NA")
sens_change$bf_c_18m[is.na(sens_change$bf_c_18m)] <- "NA"

predictors <- c("pd_motr_8m", "pd_cadapt_8m", "pd_socioem_8m", "pd_cogn_8m", "pd_com_8m")
outcomes <- c("pd_motr_28m", "pd_cadapt_28m", "pd_socioem_28m", "pd_cogn_28m", "pd_com_28m")

cohens_d_results <- data.frame( # df for storing results
  Predictor = character(),
  Outcome = character(),
  BF_Category = character(),
  Mean_8m = numeric(),
  Mean_28m = numeric(),
  Pooled_SD = numeric(),
  Cohens_d = numeric()
)

calculate_cohens_d <- function(x1, x2) { # to calculate cohens d and pooled sd
  mean_diff <- mean(x1, na.rm = TRUE) - mean(x2, na.rm = TRUE)
  sd_pooled <- sqrt(((length(x1) - 1) * var(x1, na.rm = TRUE) + 
                       (length(x2) - 1) * var(x2, na.rm = TRUE)) /
                      (length(x1) + length(x2) - 2))
  d <- mean_diff / sd_pooled
  return(list(d = d, sd_pooled = sd_pooled))
}

for (i in seq_along(predictors)) { # loop for each predictor and outcome
  predictor <- predictors[i]
  outcome <- outcomes[i]
  
  for (bf_category in unique(sens_change$bf_c_18m)) { # grouping by bf_c_18m
    sens_change_subset <- sens_change[sens_change$bf_c_18m == bf_category, ]
    x1 <- sens_change_subset[[outcome]] # outcome - 28m
    x2 <- sens_change_subset[[predictor]] # predictor - 8m
    
    if (length(x1) > 1 && length(x2) > 1) { # ensure sufficient data for calculation
      results <- calculate_cohens_d(x1, x2)
      d <- results$d
      pooled_sd <- results$sd_pooled
      mean_8m <- mean(x2, na.rm = TRUE) # calculate means and their SDs
      mean_28m <- mean(x1, na.rm = TRUE)
      sd_8m <- sd(x2, na.rm = TRUE)
      sd_28m <- sd(x1, na.rm = TRUE)
      cohens_d_results <- rbind( # add results to df
        cohens_d_results,
        data.frame(
          Predictor = predictor,
          Outcome = outcome,
          BF_Category = bf_category,
          Mean_8m = mean_8m,
          SD_8m = sd_8m,
          Mean_28m = mean_28m,
          SD_28m = sd_28m,
          Pooled_SD = pooled_sd,
          Cohens_d = d
        )
      )
    }
  }
}

write_xlsx(cohens_d_results, paste0("results/paper/tabs/", format(Sys.Date(), "%Y%m%d"), "_senschange_bf_cohensd.xlsx"))


############################# END OF THE SCRIPT #############################

model <- lm(pd_motr ~ pdmf_bayley_c_18m + b_sexo + age + age18, data = merged)
anova_model <- anova(model)

SS_effect <- anova_model["pdmf_bayley_c_18m", "Sum Sq"]
SS_total <- sum(anova_model$`Sum Sq`)
eta_squared <- SS_effect / SS_total
eta_squared







model <- lm(pd_motr ~ pdmf_bayley_c_18m, data = merged)
anova_model <- anova(model)

SS_effect <- anova_model["pdmf_bayley_c_18m", "Sum Sq"]
SS_error <- anova_model["Residuals", "Sum Sq"]
partial_eta_squared <- SS_effect / (SS_effect + SS_error)
partial_eta_squared



model <- lm(pd_motr ~ pdmf_bayley_c_18m + b_sexo + age + age18, data = merged)
anova_model <- anova(model)

SS_effect <- anova_model["pdmf_bayley_c_18m", "Sum Sq"]
SS_error <- anova_model["Residuals", "Sum Sq"]
partial_eta_squared <- SS_effect / (SS_effect + SS_error)
partial_eta_squared
