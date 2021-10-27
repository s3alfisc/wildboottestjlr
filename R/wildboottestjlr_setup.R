#' @export
wildboottestjlr_setup <- function (path_to_julia,...){
  julia <- JuliaCall::julia_setup(path_to_julia)
  #if(JuliaCall::pkg_check) JuliaCall::julia_install_package_if_needed("https://github.com/droodman/WildBootTest.jl")
  JuliaCall::julia_library("WildBootTest")
}
