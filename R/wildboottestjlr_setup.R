wildboottestjlr_setup <- function (install_julia = TRUE, install_wildboottests = TRUE, julia_path){

  #' Connect R and Julia via JuliaConnectoR & installs Julia dependency packages (if required)
  #' @param install_julia Logical. True by default. Should Julia be installed?
  #' @param install_wildboottests Logical. TRUE by default. Installs WildBootTests.jl
  #' @param julia_path Character. Path to Julia installation - required if WildBootTests.jl needs to be installed.
  #' @importFrom JuliaCall install_julia julia_install_package
  #' @export

  if(install_julia){
    JuliaCall::install_julia()
  }

  if(install_wildboottests){
    message("Note that setting up Julia & installing WildBootTests.jl will take some time - approx. 1-2 minutes.")
    JuliaCall::julia_setup(julia_path)
    JuliaCall::julia_install_package("https://github.com/droodman/WildBootTests.jl")
  }

  message("Please restart your R sessions.")



}
