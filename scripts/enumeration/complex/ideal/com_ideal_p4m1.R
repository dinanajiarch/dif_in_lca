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

## Complex - P4M1------------------------------------------------------------------------------------------------

# Define the simulation function
p4m1_func <- function(N, k, k_size, de, ex) {
  k_size_values <- if (k_size == "even") {
    c(0, 0, 0, 0)
  } else {
    c(1.609, 1.138, 0.629, 0.405)
  }
  
  sim_class  <- mplusObject(
    TITLE = glue("P4M1 - N{N} - C{k} - {k_size} - DE{de} - Ideal Separation;"),
    
    MONTECARLO = glue(
      "NAMES = u1-u10 x1 x2 x3;
    	GENERATE = u1-u10(1);
    	CATEGORICAL = u1-u10;
    	GENCLASSES = c(5);
    	CLASSES = c({k});
    	NOBSERVATIONS = {N};
    	SEED = 1495;
    	NREPS = 500;
      {ex}REPSAVE = all;
      {ex}SAVE= p4m1_N{N}_C{k}_{k_size}_de{de}_ideal*.csv;
      RESULTS = p4m1_N{N}_C{k}_{k_size}_de{de}_ideal_results.csv;"
    ),
    
    ANALYSIS =
      "TYPE = MIXTURE;
     STARTS = 200 100;",
    
    MODELPOPULATION =
      glue(
        "%OVERALL%

    [x1@0]; x1@1;
    [x2@0]; x2@1;
    [x3@0]; x3@1;

    c#1 ON x1*2;
    c#2 ON x1*2;
    c#3 ON x1*2;
    c#4 ON x1*2;

    c#1 ON x2*-1;
    c#2 ON x2*-1;
    c#3 ON x2*0;
    c#4 ON x2*0;

    c#1 ON x3*0;
    c#2 ON x3*0;
    c#3 ON x3*0;
    c#4 ON x3*0;


    [c#1*{k_size_values[1]}];
    [c#2*{k_size_values[2]}];
    [c#3*{k_size_values[3]}];
    [c#4*{k_size_values[4]}];

    u1 ON x2*{de};
    u2 ON x2*{de};

     %C#1%

  	 [u1$1*-2 u2$1*-2 u3$1*-2 u4$1*-2 u5$1*-2
  	 u6$1*-2 u7$1*-2 u8$1*-2 u9$1*-2 u10$1*-2];

     %C#2%

  	 [u1$1*-2 u2$1*-2 u3$1*-2 u4$1*-2 u5$1*-2
  	 u6$1*2 u7$1*2 u8$1*2 u9$1*2 u10$1*2];

     %C#3%

  	 [u1$1*2 u2$1*2 u3$1*2 u4$1*-2 u5$1*-2
  	 u6$1*-2 u7$1*2 u8$1*2 u9$1*2 u10$1*2];

     %C#4%

  	 [u1$1*2 u2$1*2 u3$1*2 u4$1*2 u5$1*2
  	 u6$1*-2 u7$1*-2 u8$1*-2 u9$1*-2 u10$1*-2];

     %C#5%

  	 [u1$1*2 u2$1*2 u3$1*2 u4$1*2 u5$1*2
     u6$1*2 u7$1*2 u8$1*2 u9$1*2 u10$1*2];"
      ),
    
    MODEL =
      glue(
        "
      %OVERALL%

      c on x1@0;
      c on x2@0;
      c on x3@0;

      u1 ON x2@0;
      u2 ON x2@0;

     {ex}%C#1%

  	 {ex}[u1$1*-2 u2$1*-2 u3$1*-2 u4$1*-2 u5$1*-2
  	 {ex}u6$1*-2 u7$1*-2 u8$1*-2 u9$1*-2 u10$1*-2];

     {ex}%C#2%

  	 {ex}[u1$1*-2 u2$1*-2 u3$1*-2 u4$1*-2 u5$1*-2
  	 {ex}u6$1*2 u7$1*2 u8$1*2 u9$1*2 u10$1*2];

     {ex}%C#3%

  	 {ex}[u1$1*2 u2$1*2 u3$1*2 u4$1*-2 u5$1*-2
  	 {ex}u6$1*-2 u7$1*2 u8$1*2 u9$1*2 u10$1*2];

     {ex}%C#4%

  	 {ex}[u1$1*2 u2$1*2 u3$1*2 u4$1*2 u5$1*2
  	 {ex}u6$1*-2 u7$1*-2 u8$1*-2 u9$1*-2 u10$1*-2];

     {ex}%C#5%

  	 {ex}[u1$1*2 u2$1*2 u3$1*2 u4$1*2 u5$1*2
     {ex}u6$1*2 u7$1*2 u8$1*2 u9$1*2 u10$1*2];"
      ),
    
    
    OUTPUT = "tech9 tech11;"
  )
  
  # Define output directory path
  base_dir <- here("dissertation", "complex", "p4", "m1", "ideal")
  
  # Create directory structure if it doesn't exist
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }
  
  # Ensure subdirectories are created for k_size, N, and de
  sub_dir <- glue("{k_size}/{N}/de{de}")
  full_dir <- here(base_dir, sub_dir)
  
  if (!dir.exists(full_dir)) {
    dir.create(full_dir, recursive = TRUE)
  }
  
  # Run Mplus model with dynamic file paths
  sim_fit <- mplusModeler(
    sim_class,
    dataout = glue(here(full_dir, "p4m1.dat")),
    modelout = glue(here(full_dir, "p4m1_c{k}.inp")),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE
  )
  
  return(sim_fit)
}


# Define simulation conditions
p4 <- expand.grid(
  N = c(200, 500, 5000),
  k = c(2:6),
  de = c(.405, .90, 1.50),
  k_size = c("even", "uneven")
) %>%
  arrange(k_size, N) 

# This is so we create the datasets for k=5 only
p4$ex <- ifelse(p4$k == 5, "", "!") 

# Set up parallel cluster
# Determine the number of conditions (tasks)
num_conditions <- nrow(p4)

# Limit cores to the available resources per node (e.g., 40 cores per node)
cores_per_node <- 40  # Adjust based on your cluster's specs
num_cores <- min(num_conditions, cores_per_node)

# Create the cluster
print(paste("Number of Cores Allocated:", num_cores))
cl <- makeCluster(num_cores)

# Export necessary functions and variables to the cluster
clusterExport(cl, c("p4m1_func", "p4"))
clusterEvalQ(cl, {
  library(MplusAutomation)
  library(glue)
  library(here)
})

start_time <- Sys.time()  # Start timer

# Run simulations in parallel using parLapply
result_list <- parLapply(cl, 1:nrow(p4), function(i) {
  p4m1_func(p4$N[i], p4$k[i], p4$k_size[i], p4$de[i], p4$ex[i])
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
