library(tidyverse)
conStudent <- src_mysql(host = "ba-isdsclass-programdev.lsu.edu",
                        port = 3306,
                        user = 'student',
                        password = 'student',
                        dbname = "isds_3105")
