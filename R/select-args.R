select_args_text <- function(fun, select = "", topic) {
  pieces <- strsplit(select, " +")[[1]]

  tryCatch(
    {
      parsed <- lapply(pieces, \(x) parse(text = x)[[1]])
      select_args(fun, parsed)
    },
    error = function(e) {
      warn_roxy_topic(topic$get_name(), "@inheritDotsParam failed", parent = e)
      character()
    }
  )
}

# Figure out which arguments that the user wants given a function and
# unevaluated list
select_args <- function(fun, select = list()) {
  check_function(fun)
  stopifnot(is.list(select))

  args <- names(formals(fun))

  # If fun is an S3 generic, add the formals from all known S3 methods
  # so that args defined only on methods are eligible.
  #
  # utils::isS3stdGeneric can fail on functions with unusual bodies, so we guard
  # it in a tryCatch.
  fun_is_s3_generic <- tryCatch(utils::isS3stdGeneric(fun),
                                error = function(e) FALSE)
  if (isTRUE(fun_is_s3_generic)) {
    fun_name <- names(fun_is_s3_generic)
    ns <- environment(fun)
    s3_methods <- tryCatch(
      utils::.S3methods(fun_name, envir=ns),
      error = function(e) character()
    )
    s3_args <- unlist(lapply(
      s3_methods,
      function(m) tryCatch(
        names(formals(m, envir=ns)),
        error = function(e) NULL
      )
    ))
    args <- union(args, s3_args)
  }

  args <- args[args != "..."]

  if (length(select) == 0) {
    return(args)
  }

  # Construct environment that allow minimal select-style semantics
  arg_idx <- as.list(setNames(seq_along(args), args))
  arg_env <- list2env(arg_idx, parent = emptyenv())
  arg_env$`:` <- `:`
  arg_env$`-` <- `-`
  arg_env$`(` <- `(`

  indices <- lapply(select, eval, envir = arg_env)
  for (i in seq_along(select)) {
    select_check(indices[[i]], select[[i]])
  }

  # If first is negative, start with all vars
  # If first is positive, start with no vars
  select <- rep(select_sign(indices[[1]]) < 0, length(args))

  for (idx in indices) {
    select[abs(idx)] <- select_sign(idx) > 0
  }

  args[select]
}

select_check <- function(x, call) {
  if (!is.numeric(x)) {
    cli::cli_abort(c(
      "Argument specification must evaluate to a numeric vector.",
      "Problem in {.code {deparse(call)}}."
    ))
  }

  if (!(all(x > 0) || all(x < 0))) {
    cli::cli_abort(
      c(
        "Argument specification must be all positive or all negative, not a mixture.",
        i = "Problem in {.code {deparse(call)}}."
      ),
      call = NULL
    )
  }

  invisible()
}

select_sign <- function(x) {
  if (all(x > 0)) {
    1
  } else if (all(x < 0)) {
    -1
  } else {
    NA
  }
}
