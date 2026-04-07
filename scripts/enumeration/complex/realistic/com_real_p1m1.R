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

## Complex - P1M1------------------------------------------------------------------------------------------------

# Define the simulation function
p1m1_func <- function(N, k, k_size) {
  k_size_values <- if (k_size == "even") {
    c(0, 0, 0, 0)
  } else {
    c(1.609, 1.138, 0.629, 0.405)
  }
  
  sim_class  <- mplusObject(
    TITLE = glue("P1M1 - N{N} - C{k} - {k_size} - Realistic Separation;"),
    
    MONTECARLO = glue(
      "NAMES = u1-u10;
   	GENERATE = u1-u10(1);
   	CATEGORICAL = u1-u10;
   	GENCLASSES = c(5);
   	CLASSES = c({k});
   	NOBSERVATIONS = {N};
   	SEED = 1495;
   	NREPS = 500;
    RESULTS = p1m1_N{N}_C{k}_{k_size}_realistic_results.csv;"
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
     [c#4*{k_size_values[4]}];

%C#1%

  [u1$1*3.19064 u2$1*3.10788 u3$1*3.83448 u4$1*3.86343 u5$1*2.00700
   u6$1*2.92895 u7$1*1.43653 u8$1*2.75844 u9$1*3.01256 u10$1*1.89987];

%C#2%

  [u1$1*1.16631 u2$1*-0.15619 u3$1*3.77549 u4$1*-0.51226 u5$1*-1.58301
   u6$1*0.11202 u7$1*-2.15707 u8$1*0.57930 u9$1*-0.28985 u10$1*-0.19538];

%C#3%

  [u1$1*-0.56244 u2$1*-0.06104 u3$1*-0.74264 u4$1*0.58389 u5$1*-1.44290
   u6$1*-15.0 u7$1*0.17762 u8$1*2.74127 u9$1*1.67876 u10$1*2.60714];

%C#4%

  [u1$1*-2.13783 u2$1*-3.46779 u3$1*-3.17454 u4$1*-2.12725 u5$1*-2.57868
   u6$1*-3.62206 u7$1*-3.27125 u8$1*-0.67695 u9$1*-1.86035 u10$1*-1.42219];

%C#5%

  [u1$1*-0.69130 u2$1*-1.27803 u3$1*-0.05214 u4$1*1.48891 u5$1*1.99572
   u6$1*1.75719 u7$1*-0.18973 u8$1*1.46395 u9$1*0.41521 u10$1*0.38082];"
      ),
    
    OUTPUT = "tech9 tech11;"
  )
  
  # Define output directory path
  base_dir <- here("dissertation", "complex", "p1", "m1", "realistic")
  
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
  N = c(200, 500, 1000, 5000),
  k = c(2:6),
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
