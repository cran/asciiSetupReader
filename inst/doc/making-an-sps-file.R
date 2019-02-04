## ------------------------------------------------------------------------
col_positions <- c("1-1", "2-3", "4-5", "6-11")

## ------------------------------------------------------------------------
col_names <- c("var1", "var2", "var3", "var4")

## ------------------------------------------------------------------------
col_labels <- c("version_number", "victim_sex", "victim_race", "state")

## ------------------------------------------------------------------------
value_labels <- c("victim_sex = ",
                  "MA = male",
                  "FE = female",
                  "UN = unknown",
                  "victim_race = ",
                  "WH = white",
                  "BL = black",
                  "IA = american indian or alaskan native",
                  "AS = asian",
                  "UN = unknown")

## ------------------------------------------------------------------------
missing_values <- c("victim_sex", 
                    "-9", 
                    "-8", 
                    "victim_race",
                    "-8")

## ----eval = FALSE--------------------------------------------------------
#  asciiSetupReader::make_sps_setup(file_name = "setup_file_example",
#                                   col_positions  = col_positions,
#                                   col_names      = col_names,
#                                   col_labels     = col_labels,
#                                   value_labels   = value_labels,
#                                   missing_values = missing_values)

