library(here)

# Run----

# 1. Acquire data
setwd(here())
source(file = "code/questionnaire.R")

# 2. Curate data
setwd(here())
source(file = "code/main.R")

# 3. Transform data
setwd(here())
source(file = "code/motivation.R")

# 4. Analyze data
setwd(here())
source(file = "code/inverse_efficiency.R")

# 5. Generate reports
setwd(here())
source(file = "code/plotting.R")

# Log----

# Log information on the current configuration of the system
setwd(here())
sink(file = "log/session_info.txt")
sessionInfo()
sink()

# Clean up----

# Remove all current environment variables
rm(list = ls())
