# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Library Path
.libPaths("~/R/x86_64-pc-linux-gnu-library/4.1")

# List of required libraries
required_libraries <- c("parallel", "MplusAutomation", "glue", "here", "tidyverse")

# Check if libraries are installed, and if not, install them
for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
}

# Load required libraries
library(parallel)
library(MplusAutomation)
library(glue)
library(here)
library(tidyverse)

## Simple - P1M1------------------------------------------------------------------------------------------------

# Define the simulation function
p1m1_func <- function(N, k, k_size) {
  k_size_values <- if (k_size == "even") {
    c(0, 0, 0)
  } else {
    c(1.609, .916, .405)
  }
  
  sim_class  <- mplusObject(
    TITLE = glue("P1M1 - N{N} - C{k} - {k_size} - Unstable Separation;"),
    
    MONTECARLO = glue(
      "NAMES = u1-u6;
   	GENERATE = u1-u6(1);
   	CATEGORICAL = u1-u6;
   	GENCLASSES = c(4);
   	CLASSES = c({k});
   	NOBSERVATIONS = {N};
   	SEED = 1495;
   	NREPS = 500;
    RESULTS = p1m1_N{N}_C{k}_{k_size}_unstable_results.csv;"
    ),
    
    ANALYSIS =
      "TYPE = MIXTURE;
    STARTS = 200 100;",
    
    MODELPOPULATION =
      glue(
        "%OVERALL%

     [c#1*{k_size_values[1]}];
     [c#2*{k_size_values[2]}];
     [c#3*{k_size_values[3]}];

    %C#1%
    
    [u1$1*-0.5 u2$1*-0.3 u3$1*-0.2 
    u4$1*0.1 u5$1*0.3 u6$1*0.2];

    %C#2%
    
    [u1$1*0.1 u2$1*-0.1 u3$1*0.2
    u4$1*-0.2 u5$1*-0.1 u6$1*0.0];

    %C#3%
    
    [u1$1*0.0 u2$1*0.1 u3$1*-0.1
    u4$1*0.2 u5$1*-0.3 u6$1*-0.2];

    %C#4%
    
    [u1$1*0.5 u2$1*0.6 u3$1*0.4 
    u4$1*0.7 u5$1*0.5 u6$1*0.6];"
      ),
    
    OUTPUT = "tech9 tech11;"
  )
  
  # Define output directory path
  base_dir <- here("dissertation", "simple", "p1", "m1", "unstable")
  
  # Create directory structure if it doesn't exist
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }
  
  # Ensure subdirectories are created for k_size
  sub_dir <- glue("{k_size}/{N}")
  full_dir <- here(base_dir, sub_dir)
  
  if (!dir.exists(full_dir)) {
    dir.create(full_dir, recursive = TRUE)
  }
  
  # Run Mplus model with dynamic file paths
  sim_fit <- mplusModeler(
    sim_class,
    dataout = glue(here(full_dir, "p1m1.dat")),
    modelout = glue(here(full_dir, "p1m1_c{k}.inp")),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
  
  return(sim_fit)
}


# Define simulation conditions
p1 <- expand.grid(
  N = c(200, 500, 5000),
  k = c(2:5),
  k_size = c("even", "uneven")
) %>%
  arrange(k_size, N)

# Set up parallel cluster
# Determine the number of conditions (tasks)
num_conditions <- nrow(p1)

# Limit cores to the available resources per node (e.g., 40 cores per node)
cores_per_node <- 40  # Adjust based on your cluster's specs
num_cores <- min(num_conditions, cores_per_node)

# Create the cluster
print(paste("Number of Cores Allocated:", num_cores))
cl <- makeCluster(num_cores)

# Export necessary functions and variables to the cluster
clusterExport(cl, c("p1m1_func", "p1"))
clusterEvalQ(cl, {
  library(MplusAutomation)
  library(glue)
  library(here)
})

start_time <- Sys.time()  # Start timer

# Run simulations in parallel using parLapply
result_list <- parLapply(cl, 1:nrow(p1), function(i) {
  p1m1_func(p1$N[i], p1$k[i], p1$k_size[i])
})

end_time <- Sys.time()

# Calculate elapsed time in seconds
elapsed_time_secs <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Convert to days, hours, minutes, and seconds
days <- floor(elapsed_time_secs / (24 * 3600))
hours <- floor((elapsed_time_secs %% (24 * 3600)) / 3600)
minutes <- floor((elapsed_time_secs %% 3600) / 60)
seconds <- round(elapsed_time_secs %% 60)

# Print the results
print(paste("Start time:", start_time))
print(paste("End time:", end_time))
print(paste("Total time elapsed:", days, "days", hours, "hours", minutes, "minutes", seconds, "seconds"))

# Stop the cluster
stopCluster(cl)
