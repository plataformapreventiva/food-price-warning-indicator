########################################
## Instalación y preparación de Ambiente
########################################

# Corre la siguiente función para instalar los paquetes usados en este repositorio
instalar <- function(paquete) {
  
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

paquetes <- c("tidyverse", "optparse", "dbplyr", "DBI", "RPostgreSQL")

lapply(paquetes, instalar);

option_list = list(
  make_option(c("--currentdate"), type="character", default="", 
              help="current date", metavar="character"),
  make_option(c("--database"), type="character", default="", 
              help="database name", metavar="character"),
  make_option(c("--user"), type="character", default="",
              help="database user", metavar="character"),
  make_option(c("--password"), type="character", default="",
              help="password for datbase user", metavar="character"),
  make_option(c("--host"), type="character", default="",
              help="database host name", metavar="character"),
  make_option(c("--pipeline"), type="character", default="",
  			  help="pipeline task", metavar="character")
);

opt_parser <- OptionParser(option_list=option_list);

opt <- tryCatch(
        {
          parse_args(opt_parser);
        },
        error=function(cond) {
            message("Error: Provide database connection arguments appropriately.")
            message(cond)
            print_help(opt_parser)
            return(NA)
        },
        warning=function(cond) {
            message("Warning:")
            message(cond)
            return(NULL)
        },
        finally={
            message("Finished attempting to parse arguments.")
        }
    )

PGDATABASE <- opt$database
POSTGRES_PASSWORD <- opt$password
POSTGRES_USER <- opt$user
PGHOST <- opt$host
PGPORT <- "5432"
pipeline_task <- opt$pipeline

con <- dbConnect(RPostgres::Postgres(),
    host = PGHOST,
    port = PGPORT,
    dbname = PGDATABASE,
    user = POSTGRES_USER,
    password = POSTGRES_PASSWORD
)

query = sprintf("drop table if exists models.%s; create table models.%s as (select * from raw.%s;",
	pipeline_task,
	pipeline_task,
	pipeline_task)

dbGetQuery(con, query)

# commit the change
dbCommit(con)

# disconnect from the database
dbDisconnect(con)
