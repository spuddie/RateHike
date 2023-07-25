rh_env <- new.env(parent = globalenv())

source("inst/nopkg/_GLOBAL_.R", local = rh_env)

attach(rh_env)

if (is.null(.__S3MethodsTable__.))
  .__S3MethodsTable__. <- new.env(parent = baseenv())

for (func in grep(".", ls(envir = rh_env), fixed = TRUE, value = TRUE))
  .__S3MethodsTable__.[[func]] <- rh_env[[func]]

rm(rh_env, func)
