set_julia_seed <- function(rng){

  #' Set a global seed in the current Julia session
  #' @param rng An integer that controls the random number generation
  #' @export

  rng_char <- paste0("rng = StableRNGs.StableRNG(", rng, ");")
  JuliaCall::julia_command(rng_char)

}
