# load packages
library(tidyverse)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

master <- read_csv("isha/Harvard Master.csv")

#AlCl


# 1. First, identify your actual column names
# Check the first file to see your column structure
first_file <- read.csv(csvs.alcl[1])
print(names(first_file))

# 2. Update these variables with your actual column names:
x <- "Latest..Wavelength..nm."  # Replace with your actual x column name
y <- "Latest..Absorbance"  # Replace with your actual y column name

# 3. Modified processing code with your column names
result_list <- lapply(seq_along(csvs.alcl), function(i) {
  df <- read.csv(csvs.alcl[i])
  
  # Check if columns exist
  if(!all(c(x, y) %in% names(df))) {
    warning(paste("Missing columns in", csvs.alcl[i]))
    return(data.frame(File=basename(csvs.alcl[i]), y_at_x416=NA))
  }
  
  # Find closest value to 416
  x_values <- df[[x]]
  y_values <- df[[y]]
  
  distances <- abs(x_values - 416)
  min_dist <- min(distances, na.rm=TRUE)
  
  if(is.infinite(min_dist)) {
    return(data.frame(File=basename(csvs.alcl[i]), y_at_x416=NA))
  }
  
  closest_y <- y_values[which.min(distances)]
  closest_x <- x_values[which.min(distances)]
  
  data.frame(
    File=basename(csvs.alcl[i]),
    x_value=closest_x,
    y_at_x416=closest_y
  )
})

# 4. Combine results
result_table <- do.call(rbind, result_list)

# 5. Show results
print(result_table)


# Load the master CSV file
master_data <- read.csv("isha/Harvard Master.csv")  # Replace with your path

# --------------------------
# STEP 2: Safety Checks
# --------------------------
# Check 1: Confirm same number of rows
if (nrow(master_data) != nrow(result_table)) {
  stop(paste("Row count mismatch! Master:", nrow(master_data), 
             "Result:", nrow(result_table)))
}

# Check 2: Verify "Flavonoids" column exists (or create it)
if (!"Flavonoids" %in% names(master_data)) {
  warning("Column 'Flavonoids' not found. Creating it.")
  master_data$Flavonoids <- NA
}

# Check 3: Spot-check first/last files (optional)
cat("\n--- Spot-Check ---\n")
cat("First file in master_data:", master_data$File[1], "\n")
cat("First file in result_table:", result_table$File[1], "\n")
cat("Last file in master_data:", master_data$File[nrow(master_data)], "\n")
cat("Last file in result_table:", result_table$File[nrow(result_table)], "\n")

# --------------------------
# STEP 3: Assign Values
# --------------------------
# Direct assignment (assuming rows are aligned)
master_data$Flavonoids <- result_table$y_at_x416

# --------------------------
# STEP 4: Validate
# --------------------------
# Check for NA values (if unexpected)
na_count <- sum(is.na(master_data$Flavonoids))
if (na_count > 0) {
  warning(paste(na_count, "NA values in 'Flavonoids'. Check input data."))
}

# Quick comparison
cat("\n--- Validation ---\n")
cat("First 3 Flavonoids values:\n")
print(head(master_data[, c("File", "Flavonoids")], 3))

# --------------------------
# STEP 5: Save
# --------------------------
# Backup original file (optional)
file.copy("master_data.csv", "master_data_backup.csv", overwrite = TRUE)

# Save updated master file
write.csv(master_data, "master_data_updated.csv", row.names = FALSE)

# --------------------------
# Final Message
# --------------------------
cat("\n✅ Done! Updated master file saved as 'master_data_updated.csv'.\n")
cat("⚠️ Backup created: 'master_data_backup.csv'.\n")







#attempt to make it cleaner (x5)


# =============================================
# ULTIMATE SOLUTION: HANDLES MISSING FILE COLUMN
# =============================================

# --------------------------
# CONFIGURATION (UPDATE THESE)
# --------------------------
x_col <- "Latest..Wavelength..nm."    # Your x-axis column name
y_col <- "Latest..Absorbance"    # Your y-axis column name
input_dir <- "isha/chemData/"    # Root directory with all CSV files
master_path <- "isha/Harvard Master.csv" # Path to master file

# Group specifications
group_config <- data.frame(
  group_id = 1:5,
  file_pattern = c("isha/chemData/AlCl/", "isha/chemData/FC/", "isha/chemData/PA/", "isha/chemData/Vanillin/", "isha/chemData/PA/"),
  target_x = c(416, 766, 430, 500, 600),
  output_col = c("Flavonoids", "Phenolics", "Saponins", "Tannins", "Terpenoids"),
  stringsAsFactors = FALSE
)

# --------------------------
# 1. FILE DISCOVERY
# --------------------------
all_files <- list.files(input_dir, pattern = "\\.csv$", 
                        recursive = TRUE, full.names = TRUE) %>%
  normalizePath() %>%
  str_replace_all("/+", "/")

# --------------------------
# 2. MASTER FILE HANDLING
# --------------------------
# Load or create master file
if(file.exists(master_path)) {
  master <- read.csv(master_path, stringsAsFactors = FALSE)
  
  
}

# Standardize filenames
master$File <- basename(master$File)

# --------------------------
# 3. PROCESSING FUNCTION
# --------------------------
extract_values <- function(files, target_x) {
  sapply(files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    if(!all(c(x_col, y_col) %in% names(df))) {
      warning(paste("Missing columns in:", f))
      return(NA)
    }
    x_vals <- df[[x_col]]
    y_vals <- df[[y_col]]
    y_vals[which.min(abs(x_vals - target_x))]
  })
}

# --------------------------
# 4. MAIN PROCESSING LOOP
# --------------------------
for(i in 1:nrow(group_config)) {
  pattern <- group_config$file_pattern[i]
  group_files <- all_files[str_detect(all_files, fixed(pattern))]
  
  cat(sprintf("\nProcessing %s (x=%d): %d files\n",
              group_config$output_col[i],
              group_config$target_x[i],
              length(group_files)))
  
  if(length(group_files) > 0) {
    updates <- data.frame(
      File = basename(group_files),
      Value = extract_values(group_files, group_config$target_x[i]),
      stringsAsFactors = FALSE
    )
    
    
  }
}

# --------------------------
# 5. OUTPUT VALIDATION
# --------------------------
# Create backup
backup_path <- sub("\\.csv$", paste0("_backup_", format(Sys.time(), "%Y%m%d"), ".csv"), master_path)
file.copy(master_path, backup_path)

# Save results
write.csv(master, master_path, row.names = FALSE)

# Validation report
cat("\n=== PROCESSING REPORT ===\n")
cat("Master file:", master_path, "\n")
cat("Backup saved:", backup_path, "\n\n")

cat("Files processed:", sum(!is.na(master[group_config$output_col])), "\n")
cat("NA counts per column:\n")
print(colSums(is.na(master[group_config$output_col])))

cat("\nFirst 3 results:\n")
print(head(master, 3))

# --------------------------
# 6. TROUBLESHOOTING CHECKS
# --------------------------
cat("\n=== TROUBLESHOOTING ===\n")
cat("Unique files in master:", length(unique(master$File)), "\n")
cat("Duplicate files:", sum(duplicated(master$File)), "\n")

# Check for unmatched files
processed_files <- unique(basename(all_files))
missing_in_master <- setdiff(processed_files, master$File)
if(length(missing_in_master) > 0) {
  cat("\nWARNING: Some processed files missing from master:\n")
  print(head(missing_in_master, 5))
}



masterxx <- read.csv("isha/Harvard Master.csv")

dataframe <- read.csv("isha/Harvard Master_backup_20250729_165149.csv")

dataframe$sampleID <- str_extract(dataframe$File, "(?<=_).*(?=\\.)")

master_final <- masterxx %>%
  select(Grid..:Nitrogen.content) %>%
  rename(sampleID = Accession.. )

master_final$sampleID <- gsub("\\*", "x", master_final$sampleID)
master_final$sampleID[master_final$sampleID %in% "392-92xB"] <- "392-92xA"

dataframe_final <- dataframe %>%
  select(-File) %>%
  pivot_longer(!sampleID, values_to = "absorbance", names_to = "chemical") %>%
  filter(!is.na(absorbance)) %>%
  pivot_wider(id_cols = sampleID, names_from = chemical, values_from = absorbance) %>%
  full_join(master_final)

write.csv(dataframe_final, "isha/Harvard Master.csv", row.names = FALSE)

