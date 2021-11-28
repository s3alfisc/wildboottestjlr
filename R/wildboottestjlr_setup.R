wildboottestjlr_setup <- function (install_jl_packages = FALSE, ...){

  #' Connect R and Julia via JuliaConnectoR & installs Julia dependency packages (if required)
  #' @param install_jl_packages Logical. If TRUE, all required Julia packages are installed
  #' @param ... additional function arguments
  #' @export

  if(install_jl_packages){
    # does WildBootTests.jl needs to be installed?
    JuliaCall::julia_install_package("https://github.com/droodman/WildBootTests.jl")
    JuliaCall::julia_install_package_if_needed("StableRNGs")
  }

  # connect R to a Julia session
  #julia <- JuliaCall::julia_setup(path_to_julia)

  # make libraraies available
  JuliaConnectoR::juliaImport("WildBootTests")
  #JuliaCall::julia_library("WildBootTests")
  #JuliaCall::julia_library("StableRNGs")

}
