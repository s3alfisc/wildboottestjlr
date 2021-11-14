wildboottestjlr_setup <- function (path_to_julia, install_jl_packages = FALSE, ...){

  #' Connect R and Julia via JuliaCall & installs Julia dependency packages (if required)
  #' @param path_to_julia The path to the local Julia installation
  #' @param install_jl_packages Logical. If TRUE, all required Julia packages are installed
  #' @param ... additional function arguments
  #' @export


  # connect R to a Julia session
  julia <- JuliaCall::julia_setup(path_to_julia)

  if(install_jl_packages){
    # does WildBootTests.jl needs to be installed?
    JuliaCall::julia_install_package_if_needed("https://github.com/droodman/WildBootTests.jl")
    JuliaCall::julia_install_package_if_needed("StableRNGs")
  }

  # make libraraies available
  JuliaCall::julia_library("WildBootTests")
  JuliaCall::julia_library("StableRNGs")

}
