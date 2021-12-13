wildboottestjlr_setup <- function(){

  #' install Julia, connect R and Julia, install and pre-compile WildBootTests.jl
  #' @importFrom usethis edit_r_environ
  #' @importFrom JuliaCall install_julia
  #' @importFrom JuliaConnectoR juliaEval
  #' @importFrom utils install.packages installed.packages


  required_packages <- c("JuliaCall", "usethis")
  missing_packages <- setdiff(required_packages, rownames(installed.packages()))

  if(length(missing_packages) != 0){
    install_packages <- readline(prompt= 'To set up Julia and wildboottestjlr via "wildboottestjlr_setup(), the "JuliaCall" and "usethis" packages need to be installed. At least one of the two packges is currently not installed. To install, type YES, else NO.' )
    if(is.logical(install_packages)){
      install.packages(missing_packages)
    }
  }


  install_julia <- readline(prompt= "Install Julia? If Yes, type TRUE, else FALSE: " )

  # if(!is.logical(install_julia)){
  #   cat("Please provide a logical: TRUE if you want to install Julia, and FALSE if not.")
  # }

  if(install_julia){
    JuliaCall::install_julia()
  }

  connect_r_julia <- readline('Have you already added your Julia path to your renviron file? If yes, type "YES", else type "NO": ')
  if(connect_r_julia == "NO"){
    message('Add JULIA_BINDIR= ".../Julia-1.X.X/bin" to your renviron file and save the changes.
    Restart your R session and rerun wildboottestjlr_setup(), but skip all completed steps.', appendLF = FALSE)
    suppressMessages(usethis::edit_r_environ())
  }

  if(connect_r_julia == "YES"){

    install_wildboottests <- readline(prompt="Install WildBootTests.jl? If Yes, type TRUE, else FALSE: " )

    if(as.logical(install_wildboottests)){
      JuliaConnectoR::juliaEval("using Pkg")
      JuliaConnectoR::juliaEval('Pkg.add("WildBootTests")')
      cat("Pre-compile WildBootTests.jl. This might take a few seconds.")
      JuliaConnectoR::juliaEval("using WildBootTests")
    }

  }


}
